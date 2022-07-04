package kstem

import (
	"bufio"
	"log"
	"os"
	"strings"
	"testing"
)

func Test(t *testing.T) {
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
