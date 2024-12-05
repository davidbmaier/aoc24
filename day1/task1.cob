       identification division.
       program-id. task.

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
           01 eof pic A(1). *> eof indicator for file read
           01 i pic 9(5). *> iterator for read loop
           01 value-1 pic 9(5).
           01 value-2 pic 9(5).
           01 diff pic 9(5).
           01 total pic 9(10).
           01 value-1-table occurs 1000 times pic 9(5).
           01 value-2-table occurs 1000 times pic 9(5).

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

      *    sort the tables
           sort value-1-table ascending
           sort value-2-table ascending

      *    iterate through tables and calculate differences
           move 1 to i
           perform 1000 times
               move value-1-table(i) to value-1
               move value-2-table(i) to value-2
               subtract value-1 from value-2 giving diff

               move function abs(diff) to diff

      d        display "diff between " value-1 " and " value-2 ": " diff

               add diff to total

      d        display "new total: " total

               add 1 to i
           end-perform

           display "final sum: " total

           goback.

           process-record.
               unstring input-line delimited by all space into
                   value-1 value-2
      d        display "read line: " value-1 " " value-2

               move value-1 to value-1-table(i)
               move value-2 to value-2-table(i)

               add 1 to i *> increase iteration counter
           exit.


