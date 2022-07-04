package kstem

/*==========================================================================
 * Copyright (c) 2005 University of Massachusetts.  All Rights Reserved.
 *
 * Use of the Lemur Toolkit for Language Modeling and Information Retrieval
 * is subject to the terms of the software license set forth in the LICENSE
 * file included with this software, and also available at
 * http://www.lemurproject.org/license.html
 *
 *==========================================================================
 */
/****************************************************************************\
 *	            Copyright (c) 1990-1995 by the                           *
 *    Applied Computing Systems Institute of Massachusetts, Inc. (ACSIOM)     *
 *			 All rights reserved.                                *
 *	       The INQUERY Software was provided by the                      *
 *	 Center for Intelligent Information Retrieval (CIIR),                *
 *       University of Massachusetts Computer Science Department,             *
 *		       Amherst, Massachusetts.                               *
 *	 For more information, contact ACSIOM at 413-545-6311                *
\****************************************************************************/
/*
  This is a stemmer that handles inflectional morphology and the
  most common forms of derivational morphology.  It first checks a
  word against the dictionary, and if it is found it leaves it alone.
  If not, it handles inflectional endings (plurals into singular form,
  and past tense and "ing" endings into present tense), and then
  conflates the most common derivational variants.

  Author: Bob Krovetz

  7/4/22 (mipak) ported to golang.
  6/16/04 (tds) Added kstem_allocate_memory, kstem_stem_to_buffer,
  and kstem_add_table_entry.  The kstem_allocate_memory/
  kstem_add_table_entry calls allow stemmer initialization
  without forcing the user to store stem dictionaries in
  flat files.
  07/29/2005 (dmf) Rewritten to be threadsafe C++.
*/

import (
	"strings"
	"unicode"
)

type stemmer struct {
	word []byte
	j    int
	k    int
	hash map[string]*dictEntry
}

type dictEntry struct {
	variant   string
	word      string
	exception bool
}

// New creates new stemmer instance,
// which loads the dictionary into memory
func New() (kstem *stemmer) {
	kstem = &stemmer{
		hash: make(map[string]*dictEntry),
	}

	kstem.loadTables()

	return
}

func (kstem *stemmer) Stemmer(term string) (result string) {
	stem_it := true
	kstem.k = len(term) - 1
	/* if the kstem.word is too long or too short, or not entirely
	   alphabetic, just lowercase copy it into stem and return */
	if (kstem.k <= 2-1) || (kstem.k >= MAX_WORD_LENGTH-1) {
		stem_it = false
	} else {
		for i := 0; i <= kstem.k; i++ {
			// 8 bit characters can be a problem on windows
			if !unicode.IsLetter(rune(term[i])) {
				stem_it = false
				break
			}
		}
	}

	if !stem_it {
		return term
	}

	/* 'kstem.word' is a pointer, global to this file, for manipulating the kstem.word in
	   the buffer provided through the passed in pointer 'stem'. */
	kstem.word = make([]byte, MAX_WORD_LENGTH)

	/* lowercase the local copy */
	for i := 0; i <= kstem.k; i++ {
		kstem.word[i] = byte(unicode.ToLower(rune(term[i])))
	}

	kstem.word[kstem.k+1] = []byte{0}[0]

	/* the basic algorithm is to check the dictionary, and leave the kstem.word as
	   it is if the kstem.word is found. Otherwise, recognize plurals, tense, etc.
	   and normalize according to the rules for those affixes.  Check against
	   the dictionary after each stage, so `longings' -> `longing' rather than
	   `long'. Finally, deal with some derivational endings.  The -ion, -er,
	   and -ly endings must be checked before -ize.  The -ity ending must come
	   before -al, and -ness must come before -ly and -ive.  Finally, -ncy must
	   come before -nce (because -ncy is converted to -nce for some instances).
	*/

	/* This while loop will never repeat; it is only here to allow the
	   break statement to be used to escape as soon as a kstem.word is recognized.
	*/
	var dep *dictEntry
	for {
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.plural()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.past_tense()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.aspect()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ity_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ness_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ion_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.er_and_or_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ly_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.al_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ive_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ize_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ment_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ble_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ism_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ic_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.ncy_endings()
		if dep = kstem.getdep(kstem.word); dep != nil {
			break
		}
		kstem.nce_endings()
		dep = kstem.getdep(kstem.word)
		break
	}

	if dep != nil && dep.word != "" {
		return dep.word
	}

	return string(kstem.word[:kstem.k+1])
}

/* Adds a stem entry into the hash table; forces the stemmer to stem
 * <variant> to <kstem.word>.  If <kstem.word> == "", <variant> is stemmed to itself.
 */
func (kstem *stemmer) add_table_entry(variant string, word string, exc bool) {
	kstem.hash[variant] = &dictEntry{
		variant:   variant,
		word:      word,
		exception: exc,
	}
}

/* the length of kstem.word (not an lvalue) */
func (kstem *stemmer) wordlength() int {
	return kstem.k + 1
}

/* length of stem within kstem.word (not an lvalue) */
func (kstem *stemmer) stemlength() int {
	return kstem.j + 1
}

/* the last character of kstem.word */
func (kstem *stemmer) final_c() byte {
	return kstem.word[kstem.k]
}

/* the penultimate character of kstem.word */
func (kstem *stemmer) penult_c() byte {
	return kstem.word[kstem.k-1]
}

/* getdep(kstem.word) returns NULL if kstem.word is not found in the dictionary,
   and returns a pointer to a dictEntry if found  */
func (kstem *stemmer) getdep(word []byte) (entry *dictEntry) {
	/* don't bother to check for words that are short */
	if len(word) <= 1 {
		return nil
	}

	entry, _ = kstem.hash[string(kstem.word[:kstem.k+1])]

	return
}

/* lookup(kstem.word) returns false if kstem.word is not found in the dictionary,
   and true if it is */
func (kstem *stemmer) lookup(word []byte) bool {
	if kstem.getdep(word) != nil {
		return true
	}
	return false
}

/* cons() returns TRUE if kstem.word[i] is a consonant. */
func (kstem *stemmer) cons(i int) bool {
	ch := kstem.word[i]
	if ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u' {
		return false
	}

	if ch != 'y' || i == 0 {
		return true
	} else {
		/* ch == y, test previous char. If vowel, y is consonant
		   the case of yy (previously handled via recursion) is ignored.
		*/
		ch = kstem.word[i-1]
		return ch == 'a' || ch == 'e' || ch == 'i' || ch == 'o' || ch == 'u'
	}
}

func (kstem *stemmer) vowel(i int) bool {
	return !kstem.cons(i)
}

/* This routine is useful for ensuring that we don't stem acronyms */
func (kstem *stemmer) vowelinstem() bool {
	for i := 0; i <= kstem.stemlength(); i++ {
		if kstem.vowel(i) {
			return true
		}
	}
	return false
}

/* return TRUE if kstem.word ends with a double consonant */
func (kstem *stemmer) doublec(i int) bool {
	if i < 1 {
		return false
	}

	if kstem.word[i] != kstem.word[i-1] {
		return false
	}

	return kstem.cons(i)
}

/* Passing the length of str is awkward, but important for performance.  Since
   str is always a string constant, we can define a macro ends_in (see the macro
   section of this module) which takes str and determines its length at compile
   time.  Note that str must therefore no longer be padded with spaces in the calls
   to ends_in (as it was in the original version of this code).
*/
func (kstem *stemmer) ends(str string, sufflength int) (match bool) {
	r := kstem.wordlength() - sufflength

	match = strings.HasSuffix(string(kstem.word[:kstem.k+1]), str)

	/* use r-1 since kstem.j is an index rather than length */
	if match {
		kstem.j = r - 1
	} else {
		kstem.j = kstem.k
	}
	return match
}

func (kstem *stemmer) ends_in(str string) bool {
	return kstem.ends(str, len(str))
}

/* replace old suffix with str */
func (kstem *stemmer) setsuff(str string, length int) {
	kstem.word = kstem.word[:kstem.j+1]
	kstem.word = append(kstem.word, str...)

	// add 0 to fix length
	for i := len(kstem.word); i < MAX_WORD_LENGTH; i++ {
		kstem.word = append(kstem.word, 0)
	}

	kstem.k = kstem.j + length
	kstem.word[kstem.k+1] = 0
}

func (kstem *stemmer) setsuffix(str string) {
	kstem.setsuff(str, len(str))
}

/* convert plurals to singular form, and `-ies' to `y' */
func (kstem *stemmer) plural() {
	if kstem.final_c() == 's' {
		if kstem.ends_in("ies") {
			kstem.word[kstem.j+3] = 0
			kstem.k--
			if kstem.lookup(kstem.word) {
				/* ensure calories -> calorie */
				return
			}
			kstem.k++
			kstem.word[kstem.j+3] = 's'
			kstem.setsuffix("y")
		} else if kstem.ends_in("es") {
			/* try just removing the "s" */
			kstem.word[kstem.j+2] = 0
			kstem.k--

			/* note: don't check for exceptions here.  So, `aides' -> `aide',
			   but `aided' -> `aid'.  The exception for double s is used to prevent
			   crosses -> crosse.  This is actually correct if crosses is a plural
			   noun (a type of racket used in lacrosse), but the verb is much more
			   common */

			if kstem.lookup(kstem.word) && kstem.j > 0 && !(kstem.word[kstem.j] == 's' && kstem.word[kstem.j-1] == 's') {
				return
			}

			/* try removing the "es" */

			kstem.word[kstem.j+1] = 0
			kstem.k--
			if kstem.lookup(kstem.word) {
				return
			}

			/* the default is to retain the "e" */
			kstem.word[kstem.j+1] = 'e'
			kstem.word[kstem.j+2] = 0
			kstem.k++
			return
		} else {
			if kstem.wordlength() > 3 && kstem.penult_c() != 's' && !kstem.ends_in("ous") {
				/* unless the kstem.word ends in "ous" or a double "s", remove the final "s" */
				kstem.word[kstem.k] = 0
				kstem.k--
			}
		}
	}
}

/* convert past tense (-ed) to present, and `-ied' to `y' */
func (kstem *stemmer) past_tense() {
	/* Handle words less than 5 letters with a direct mapping
	   This prevents (fled -> fl).  */

	if kstem.wordlength() <= 4 {
		return
	}

	if kstem.ends_in("ied") {
		kstem.word[kstem.j+3] = 0
		kstem.k--
		if kstem.lookup(kstem.word) { /* we almost always want to convert -ied to -y, but */
			return /* this isn't true for short words (died->die)      */
		}
		kstem.k++                   /* I don't know any long words that this applies to, */
		kstem.word[kstem.j+3] = 'd' /* but just in case...                              */
		kstem.setsuffix("y")
		return
	}

	/* the vowelinstem() is necessary so we don't stem acronyms */
	if kstem.ends_in("ed") && kstem.vowelinstem() {
		/* see if the root ends in `e' */
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1

		if dep := kstem.getdep(kstem.word); dep != nil {
			/* if it's in the dictionary and not an exception */
			if !dep.exception {
				return
			}
		}

		/* try removing the "ed" */
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}

		/* try removing a doubled consonant.  if the root isn't found in
		   the dictionary, the default is to leave it doubled.  This will
		   correctly capture `backfilled' -> `backfill' instead of
		   `backfill' -> `backfille', and seems correct most of the time  */

		if kstem.doublec(kstem.k) {
			kstem.word[kstem.k] = 0
			kstem.k--
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.k+1] = kstem.word[kstem.k]
			kstem.k++
			return
		}

		/* if we have a `un-' prefix, then leave the kstem.word alone  */
		/* (this will sometimes screw up with `under-', but we   */
		/*  will take care of that later)                        */

		if (kstem.word[0] == 'u') && (kstem.word[1] == 'n') {
			kstem.word[kstem.k+1] = 'e'
			kstem.word[kstem.k+2] = 'd'
			kstem.k = kstem.k + 2
			return
		}

		/* it wasn't found by just removing the `d' or the `ed', so prefer to
		   end with an `e' (e.g., `microcoded' -> `microcode'). */

		kstem.word[kstem.j+1] = 'e'
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		return
	}

}

/* handle `-ing' endings */
func (kstem *stemmer) aspect() {
	/* handle short words (aging -> age) via a direct mapping.  This
	   prevents (thing -> the) in the version of this routine that
	   ignores inflectional variants that are mentioned in the dictionary
	   (when the root is also present) */

	if kstem.wordlength() <= 5 {
		return
	}

	/* the vowelinstem() is necessary so we don't stem acronyms */
	if kstem.ends_in("ing") && kstem.vowelinstem() {

		/* try adding an `e' to the stem and check against the dictionary */
		kstem.word[kstem.j+1] = 'e'
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1

		if dep := kstem.getdep(kstem.word); dep != nil {
			if !dep.exception { /* if it's in the dictionary and not an exception */
				return
			}
		}

		/* adding on the `e' didn't work, so remove it */
		kstem.word[kstem.k] = 0
		kstem.k-- /* note that `ing' has also been removed */

		if kstem.lookup(kstem.word) {
			return
		}

		/* if I can remove a doubled consonant and get a kstem.word, then do so */
		if kstem.doublec(kstem.k) {
			kstem.k--
			kstem.word[kstem.k+1] = 0
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.k+1] = kstem.word[kstem.k] /* restore the doubled consonant */

			/* the default is to leave the consonant doubled            */
			/*  (e.g.,`fingerspelling' -> `fingerspell').  Unfortunately */
			/*  `bookselling' -> `booksell' and `mislabelling' -> `mislabell'). */
			/*  Without making the algorithm significantly more complicated, this */
			/*  is the best I can do */
			kstem.k++
			return
		}

		/* the kstem.word wasn't in the dictionary after removing the stem, and then
		   checking with and without a final `e'.  The default is to add an `e'
		   unless the kstem.word ends in two consonants, so `microcoding' -> `microcode'.
		   The two consonants restriction wouldn't normally be necessary, but is
		   needed because we don't try to deal with prefixes and compounds, and
		   most of the time it is correct (e.g., footstamping -> footstamp, not
		   footstampe; however, decoupled -> decoupl).  We can prevent almost all
		   of the incorrect stems if we try to do some prefix analysis first */

		if kstem.j > 0 && kstem.cons(kstem.j) && kstem.cons(kstem.j-1) {
			kstem.k = kstem.j
			kstem.word[kstem.k+1] = 0
			return
		}

		kstem.word[kstem.j+1] = 'e'
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		return
	}
}

/* handle some derivational endings */

/* this routine deals with -ion, -ition, -ation, -ization, and -ication.  The
   -ization ending is always converted to -ize */
func (kstem *stemmer) ion_endings() {
	old_k := kstem.k

	if kstem.ends_in("ization") { /* the -ize ending is very productive, so simply accept it as the root */
		kstem.word[kstem.j+3] = 'e'
		kstem.word[kstem.j+4] = 0
		kstem.k = kstem.j + 3
		return
	}

	if kstem.ends_in("ition") {
		kstem.word[kstem.j+1] = 'e'
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) { /* remove -ition and add `e', and check against the dictionary */
			return /* (e.g., definition->define, opposition->oppose) */
		}

		/* restore original values */
		kstem.word[kstem.j+1] = 'i'
		kstem.word[kstem.j+2] = 't'
		kstem.k = old_k
	}

	if kstem.ends_in("ation") {
		kstem.word[kstem.j+3] = 'e'
		kstem.word[kstem.j+4] = 0
		kstem.k = kstem.j + 3
		if kstem.lookup(kstem.word) { /* remove -ion and add `e', and check against the dictionary */
			return /* (elmination -> eliminate)  */
		}

		kstem.word[kstem.j+1] = 'e' /* remove -ation and add `e', and check against the dictionary */
		kstem.word[kstem.j+2] = 0   /* (allegation -> allege) */
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = 0 /* just remove -ation (resignation->resign) and check dictionary */
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}

		/* restore original values */
		kstem.word[kstem.j+1] = 'a'
		kstem.word[kstem.j+2] = 't'
		kstem.word[kstem.j+3] = 'i'
		kstem.word[kstem.j+4] = 'o' /* no need to restore kstem.kstem.kstem.word[kstem.kstem.kstem.j+5] (n); it was never changed */
		kstem.k = old_k
	}

	/* test -ication after -ation is attempted (e.g., `complication->complicate'
	   rather than `complication->comply') */

	if kstem.ends_in("ication") {
		kstem.word[kstem.j+1] = 'y'
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) { /* remove -ication and add `y', and check against the dictionary */
			return /* (e.g., amplification -> amplify) */
		}

		/* restore original values */
		kstem.word[kstem.j+1] = 'i'
		kstem.word[kstem.j+2] = 'c'
		kstem.k = old_k
	}

	if kstem.ends_in("ion") {
		kstem.word[kstem.j+1] = 'e'
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) { /* remove -ion and add `e', and check against the dictionary */
			return
		}

		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) { /* remove -ion, and if it's found, treat that as the root */
			return
		}

		/* restore original values */
		kstem.word[kstem.j+1] = 'i'
		kstem.word[kstem.j+2] = 'o'
		kstem.k = old_k
	}

	return
}

/* this routine deals with -er, -or, -ier, and -eer.  The -izer ending is always converted to
   -ize */

func (kstem *stemmer) er_and_or_endings() {
	old_k := kstem.k

	var word_char byte /* so we can remember if it was -er or -or */

	if kstem.ends_in("izer") { /* -ize is very productive, so accept it as the root */
		kstem.word[kstem.j+4] = 0
		kstem.k = kstem.j + 3
		return
	}

	if kstem.ends_in("er") || kstem.ends_in("or") {
		word_char = kstem.word[kstem.j+1]
		if kstem.doublec(kstem.j) {
			kstem.word[kstem.j] = 0
			kstem.k = kstem.j - 1
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.j] = kstem.word[kstem.j-1] /* restore the doubled consonant */
		}

		if kstem.word[kstem.j] == 'i' { /* do we have a -ier ending? */
			kstem.word[kstem.j] = 'y'
			kstem.word[kstem.j+1] = 0
			kstem.k = kstem.j
			if kstem.lookup(kstem.word) { /* yes, so check against the dictionary */
				return
			}
			kstem.word[kstem.j] = 'i' /* restore the endings */
			kstem.word[kstem.j+1] = 'e'
		}

		if kstem.word[kstem.j] == 'e' { /* handle -eer */
			kstem.word[kstem.j] = 0
			kstem.k = kstem.j - 1
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.j] = 'e'
		}

		kstem.word[kstem.j+2] = 0 /* remove the -r ending */
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+1] = 0 /* try removing -er/-or */
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+1] = 'e' /* try removing -or and adding -e */
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = word_char /* restore the kstem.kstem.word to the way it was */
		kstem.word[kstem.j+2] = 'r'
		kstem.k = old_k
	}

}

/* this routine deals with -ly endings.  The -ally ending is always converted to -al
   Sometimes this will temporarily leave us with a non-kstem.word (e.g., heuristically
   maps to heuristical), but then the -al is removed in the next step.  */

func (kstem *stemmer) ly_endings() {
	old_k := kstem.k

	if kstem.ends_in("ly") {
		kstem.word[kstem.j+2] = 'e' /* try converting -ly to -le */
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+2] = 'y'

		kstem.word[kstem.j+1] = 0 /* try just removing the -ly */
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}
		if kstem.j > 0 && (kstem.word[kstem.j-1] == 'a') && (kstem.word[kstem.j] == 'l') { /* always convert -ally to -al */
			return
		}
		kstem.word[kstem.j+1] = 'l'
		kstem.k = old_k

		if kstem.j > 0 && (kstem.word[kstem.j-1] == 'a') && (kstem.word[kstem.j] == 'b') { /* always convert -ably to -able */
			kstem.word[kstem.j+2] = 'e'
			kstem.k = kstem.j + 2
			return
		}

		if kstem.word[kstem.j] == 'i' { /* e.g., militarily -> military */
			kstem.word[kstem.j] = 'y'
			kstem.word[kstem.j+1] = 0
			kstem.k = kstem.j
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.j] = 'i'
			kstem.word[kstem.j+1] = 'l'
			kstem.k = old_k
		}

		kstem.word[kstem.j+1] = 0 /* the default is to remove -ly */
		kstem.k = kstem.j
	}
	return
}

/* this routine deals with -al endings.  Some of the endings from the previous routine
   are finished up here.  */

func (kstem *stemmer) al_endings() {
	old_k := kstem.k

	if kstem.ends_in("al") {
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) { /* try just removing the -al */
			return
		}

		if kstem.doublec(kstem.j) { /* allow for a doubled consonant */
			kstem.word[kstem.j] = 0
			kstem.k = kstem.j - 1
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.j] = kstem.word[kstem.j-1]
		}

		kstem.word[kstem.j+1] = 'e' /* try removing the -al and adding -e */
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = 'u' /* try converting -al to -um */
		kstem.word[kstem.j+2] = 'm' /* (e.g., optimal - > optimum ) */
		kstem.k = kstem.j + 2
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = 'a' /* restore the ending to the way it was */
		kstem.word[kstem.j+2] = 'l'
		kstem.word[kstem.j+3] = 0
		kstem.k = old_k

		if kstem.j > 0 && (kstem.word[kstem.j-1] == 'i') && (kstem.word[kstem.j] == 'c') {
			kstem.word[kstem.j-1] = 0 /* try removing -ical  */
			kstem.k = kstem.j - 2
			if kstem.lookup(kstem.word) {
				return
			}

			kstem.word[kstem.j-1] = 'y' /* try turning -ical to -y (e.g., bibliographical) */
			kstem.word[kstem.j] = 0
			kstem.k = kstem.j - 1
			if kstem.lookup(kstem.word) {
				return
			}

			kstem.word[kstem.j-1] = 'i'
			kstem.word[kstem.j] = 'c'
			kstem.word[kstem.j+1] = 0 /* the default is to convert -ical to -ic */
			kstem.k = kstem.j
			return
		}

		if kstem.word[kstem.j] == 'i' { /* sometimes -ial endings should be removed */
			kstem.word[kstem.j] = 0 /* (sometimes it gets turned into -y, but we */
			kstem.k = kstem.j - 1   /* aren't dealing with that case for now) */
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.j] = 'i'
			kstem.k = old_k
		}

	}
	return
}

/* this routine deals with -ive endings.  It normalizes some of the
   -ative endings directly, and also maps some -ive endings to -ion. */

func (kstem *stemmer) ive_endings() {
	old_k := kstem.k

	if kstem.ends_in("ive") {
		kstem.word[kstem.j+1] = 0 /* try removing -ive entirely */
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = 'e' /* try removing -ive and adding -e */
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+1] = 'i'
		kstem.word[kstem.j+2] = 'v'

		if kstem.j > 0 && (kstem.word[kstem.j-1] == 'a') && (kstem.word[kstem.j] == 't') {
			kstem.word[kstem.j-1] = 'e' /* try removing -ative and adding -e */
			kstem.word[kstem.j] = 0     /* (e.g., determinative -> determine) */
			kstem.k = kstem.j - 1
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.j-1] = 0 /* try just removing -ative */
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.j-1] = 'a'
			kstem.word[kstem.j] = 't'
			kstem.k = old_k
		}

		/* try mapping -ive to -ion (e.g., injunctive/injunction) */
		kstem.word[kstem.j+2] = 'o'
		kstem.word[kstem.j+3] = 'n'
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+2] = 'v' /* restore the original values */
		kstem.word[kstem.j+3] = 'e'
		kstem.k = old_k
	}
	return
}

/* this routine deals with -ize endings. */

func (kstem *stemmer) ize_endings() {
	old_k := kstem.k

	if kstem.ends_in("ize") {
		kstem.word[kstem.j+1] = 0 /* try removing -ize entirely */
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+1] = 'i'

		if kstem.doublec(kstem.j) { /* allow for a doubled consonant */
			kstem.word[kstem.j] = 0
			kstem.k = kstem.j - 1
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.word[kstem.j] = kstem.word[kstem.j-1]
		}

		kstem.word[kstem.j+1] = 'e' /* try removing -ize and adding -e */
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+1] = 'i'
		kstem.word[kstem.j+2] = 'z'
		kstem.k = old_k
	}
	return
}

/* this routine deals with -ment endings. */

func (kstem *stemmer) ment_endings() {
	old_k := kstem.k

	if kstem.ends_in("ment") {
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+1] = 'm'
		kstem.k = old_k
	}
	return
}

/* handle -able and -ible */
func (kstem *stemmer) ble_endings() {
	old_k := kstem.k
	var word_char byte

	if kstem.ends_in("ble") {
		if !((kstem.word[kstem.j] == 'a') || (kstem.word[kstem.j] == 'i')) {
			return
		}
		word_char = kstem.word[kstem.j]
		kstem.word[kstem.j] = 0 /* try just removing the ending */
		kstem.k = kstem.j - 1
		if kstem.lookup(kstem.word) {
			return
		}
		if kstem.doublec(kstem.k) { /* allow for a doubled consonant */
			kstem.word[kstem.k] = 0
			kstem.k--
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.k++
			kstem.word[kstem.k] = kstem.word[kstem.k-1]
		}
		kstem.word[kstem.j] = 'e' /* try removing -a/ible and adding -e */
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j] = 'a'   /* try removing -able and adding -ate */
		kstem.word[kstem.j+1] = 't' /* (e.g., compensable/compensate)     */
		kstem.word[kstem.j+2] = 'e'
		kstem.word[kstem.j+3] = 0
		kstem.k = kstem.j + 2
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j] = word_char /* restore the original values */
		kstem.word[kstem.j+1] = 'b'
		kstem.word[kstem.j+2] = 'l'
		kstem.word[kstem.j+3] = 'e'
		kstem.k = old_k
	}
	return
}

/* this routine deals with -ity endings.  It accepts -ability, -ibility,
   and -ality, even without checking the dictionary because they are so
   productive.  The first two are mapped to -ble, and the -ity is remove
   for the latter */
func (kstem *stemmer) ity_endings() {
	old_k := kstem.k

	if kstem.ends_in("ity") {
		kstem.word[kstem.j+1] = 0 /* try just removing -ity */
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+1] = 'e' /* try removing -ity and adding -e */
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j+1] = 'i'
		kstem.word[kstem.j+2] = 't'
		kstem.k = old_k

		/* the -ability and -ibility endings are highly productive, so just accept them */
		if kstem.j > 0 && (kstem.word[kstem.j-1] == 'i') && (kstem.word[kstem.j] == 'l') {
			kstem.word[kstem.j-1] = 'l' /* convert to -ble */
			kstem.word[kstem.j] = 'e'
			kstem.word[kstem.j+1] = 0
			kstem.k = kstem.j
			return
		}

		/* ditto for -ivity */
		if kstem.j > 0 && (kstem.word[kstem.j-1] == 'i') && (kstem.word[kstem.j] == 'v') {
			kstem.word[kstem.j+1] = 'e' /* convert to -ive */
			kstem.word[kstem.j+2] = 0
			kstem.k = kstem.j + 1
			return
		}

		/* ditto for -ality */
		if kstem.j > 0 && (kstem.word[kstem.j-1] == 'a') && (kstem.word[kstem.j] == 'l') {
			kstem.word[kstem.j+1] = 0
			kstem.k = kstem.j
			return
		}

		/* if the root isn't in the dictionary, and the variant *is*
		   there, then use the variant.  This allows `immunity'->`immune',
		   but prevents `capacity'->`capac'.  If neither the variant nor
		   the root form are in the dictionary, then remove the ending
		   as a default */

		if kstem.lookup(kstem.word) {
			return
		}

		/* the default is to remove -ity altogether */
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		return
	}
}

/* handle -able and -ible */
func (kstem *stemmer) ble_ending() {
	old_k := kstem.k
	var word_char byte

	if kstem.ends_in("ble") {
		if !(kstem.word[kstem.j] == 'a' || kstem.word[kstem.j] == 'i') {
			return
		}
		word_char = kstem.word[kstem.j]
		kstem.word[kstem.j] = 0 /* try just removing the ending */
		kstem.k = kstem.j - 1
		if kstem.lookup(kstem.word) {
			return
		}
		if kstem.doublec(kstem.k) { /* allow for a doubled consonant */
			kstem.word[kstem.k] = 0
			kstem.k--
			if kstem.lookup(kstem.word) {
				return
			}
			kstem.k++
			kstem.word[kstem.k] = kstem.word[kstem.k-1]
		}
		kstem.word[kstem.j] = 'e' /* try removing -a/ible and adding -e */
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j] = 'a'   /* try removing -able and adding -ate */
		kstem.word[kstem.j+1] = 't' /* (e.g., compensable/compensate)     */
		kstem.word[kstem.j+2] = 'e'
		kstem.word[kstem.j+3] = 0
		kstem.k = kstem.j + 2
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j] = word_char /* restore the original values */
		kstem.word[kstem.j+1] = 'b'
		kstem.word[kstem.j+2] = 'l'
		kstem.word[kstem.j+3] = 'e'
		kstem.k = old_k
	}
	return
}

/* handle -ness */
func (kstem *stemmer) ness_endings() {
	if kstem.ends_in("ness") { /* this is a very productive endings, so just accept it */
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		if kstem.word[kstem.j] == 'i' {
			kstem.word[kstem.j] = 'y'
		}
	}
	return
}

/* handle -ism */
func (kstem *stemmer) ism_endings() {
	if kstem.ends_in("ism") { /* this is a very productive ending, so just accept it */
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
	}
	return
}

/* handle -ic endings.   This is fairly straightforward, but this is
   also the only place we try *expanding* an ending, -ic -> -ical.
   This is to handle cases like `canonic' -> `canonical' */

func (kstem *stemmer) ic_endings() {
	if kstem.ends_in("ic") {
		kstem.word[kstem.j+3] = 'a' /* try converting -ic to -ical */
		kstem.word[kstem.j+4] = 'l'
		kstem.word[kstem.j+5] = 0
		kstem.k = kstem.j + 4
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = 'y' /* try converting -ic to -y */
		kstem.word[kstem.j+2] = 0
		kstem.k = kstem.j + 1
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = 'e' /* try converting -ic to -e */
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = 0 /* try removing -ic altogether */
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+1] = 'i' /* restore the original ending */
		kstem.word[kstem.j+2] = 'c'
		kstem.word[kstem.j+3] = 0
		kstem.k = kstem.j + 2
	}
	return
}

/* handle -ency and -ancy */
func (kstem *stemmer) ncy_endings() {
	if kstem.ends_in("ncy") {
		if !((kstem.word[kstem.j] == 'e') || (kstem.word[kstem.j] == 'a')) {
			return
		}
		kstem.word[kstem.j+2] = 't' /* try converting -ncy to -nt */
		kstem.word[kstem.j+3] = 0   /* (e.g., constituency -> constituent) */
		kstem.k = kstem.j + 2

		if kstem.lookup(kstem.word) {
			return
		}

		kstem.word[kstem.j+2] = 'c' /* the default is to convert it to -nce */
		kstem.word[kstem.j+3] = 'e'
		kstem.k = kstem.j + 3
	}
	return
}

/* handle -ence and -ance */
func (kstem *stemmer) nce_endings() {
	old_k := kstem.k

	var word_char byte

	if kstem.ends_in("nce") {
		if !((kstem.word[kstem.j] == 'e') || (kstem.word[kstem.j] == 'a')) {
			return
		}
		word_char = kstem.word[kstem.j]
		kstem.word[kstem.j] = 'e' /* try converting -e/ance to -e (adherance/adhere) */
		kstem.word[kstem.j+1] = 0
		kstem.k = kstem.j
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j] = 0 /* try removing -e/ance altogether (disappearance/disappear) */
		kstem.k = kstem.j - 1
		if kstem.lookup(kstem.word) {
			return
		}
		kstem.word[kstem.j] = word_char /* restore the original ending */
		kstem.word[kstem.j+1] = 'n'
		kstem.k = old_k
	}
	return
}

func (kstem *stemmer) loadTables() {
	/* Initialize hash table  */
	for i := range exceptionWords {
		kstem.add_table_entry(exceptionWords[i], "", true)
	}
	for i := range headwords {
		kstem.add_table_entry(headwords[i], "", false)
	}
	for i := range supplementDict {
		kstem.add_table_entry(supplementDict[i], "", false)
	}
	for i := range properNouns {
		kstem.add_table_entry(properNouns[i], "", false)
	}
	for i := range directConflations {
		kstem.add_table_entry(directConflations[i].variant, directConflations[i].word, false)
	}
	for i := range countryNationality {
		kstem.add_table_entry(countryNationality[i].variant, countryNationality[i].word, false)
	}
}
