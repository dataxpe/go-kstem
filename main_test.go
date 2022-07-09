package kstem

import (
	"bufio"
	"log"
	"os"
	"strings"
	"sync"
	"testing"
)

func TestSingle(t *testing.T) {
	file, err := os.Open("./test/kstem_examples.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	s := New()

	var tested int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " ")
		if len(line) >= 2 {
			res := s.Stemmer(line[0])
			if res != line[1] {
				t.Errorf("'%s': Expected '%s' but got '%s'", line[0], line[1], res)
				break
			}
			tested++
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
}

func TestConcurrent(t *testing.T) {
	file, err := os.Open("./test/kstem_examples.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	s := New()

	var wg sync.WaitGroup
	var tested int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " ")
		if len(line) >= 2 {
			wg.Add(1)
			go func() {
				res := s.Stemmer(line[0])
				if res != line[1] {
					t.Errorf("'%s': Expected '%s' but got '%s'", line[0], line[1], res)
				}
				tested++
				wg.Done()
			}()
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

	wg.Wait()
}
