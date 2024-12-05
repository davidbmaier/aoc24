       identification division.
       program-id. task1.

       environment division.
           input-output section.
           file-control.
           select task-input assign to "input"
           organization is line sequential.

       data division.
           file section.
           fd task-input.
           01 input-line-file pic X(13).

           working-storage section.
           01 input-line pic X(13).
           01 eof pic A(1). *> indicator for end of file
           01 eot pic A(1). *> indicator for end of table
           01 i pic 9(5). *> iterator for read loop

           01 value-1 pic 9(5).
           01 value-2 pic 9(5).
           01 occurrence-counter pic 9(4).
           01 product pic 9(10).
           01 total pic 9(10).

      *    input has 1000 entries
           01 value-1-table occurs 1000 times pic 9(5).
           01 value-2-table occurs 1000 times indexed search-index
               pic 9(5).

       procedure division.
           move 1 to i *> first table index

           open input task-input
           perform until eof='E'
               read task-input into input-line
                   at end
                       move 'E' to eof
                   not at end
                       perform process-record
               end-read
           end-perform
           close task-input

      *    iterate through first table and find occurrences
           move 1 to i
           perform 1000 times *> 1000 entries in the input
               move value-1-table(i) to value-1

               set search-index to 1
               move 0 to occurrence-counter
               move 'N' to eot *> reset end of table indicator

      *        search repeatedly and increment the counter for matches
               perform until eot='E'
                   search value-2-table varying search-index
                       at end
                           move 'E' to eot
                       when value-2-table(search-index) = value-1
                           add 1 to occurrence-counter
      *                    need to manually increase index
                           add 1 to search-index
                   end-search
               end-perform

      d        display "found " occurrence-counter " occurrences for "
      d            value-1

      *        calculate product and add to total sum
               multiply value-1 by occurrence-counter giving product
               add product to total

               add 1 to i
           end-perform

           display "total sum: " total

           goback.

           process-record.
               unstring input-line delimited by all space into
                   value-1 value-2
      d        display "read line: " value-1 " " value-2

               move value-1 to value-1-table(i)
               move value-2 to value-2-table(i)

               add 1 to i *> increase iteration counter
           exit.


