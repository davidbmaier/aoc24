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
           01 input-line-file pic X(100).

           working-storage section.
           01 input-line pic X(100).
           01 eof pic A(1). *> eof indicator for file read
           01 unstring-fields.
               05 table-index pic 9(3).
               05 line-index pic 9(3).
               05 start-index pic 9(3).
           01 report-table occurs 10 times pic 9(2).
           01 report-fields.
               05 direction pic A(1).
               05 value-1 pic 9(2).
               05 value-2 pic 9(2).
               05 diff pic S9(2).
               05 safe pic A(1).
               05 report-index pic 9(3).
           01 total pic 9(5).

       procedure division.
           move 0 to total

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

           display "total safe: " total

           goback.

           process-record.
      d        display input-line
               move 1 to table-index
               move 1 to start-index
               move "Y" to safe

               perform varying line-index from 1 by 1
                       until line-index > function length(input-line)

      d            display line-index ": " input-line(line-index:1)
                   if input-line(line-index:1) = " "
                       move input-line(start-index:2)
                           to report-table(table-index)
                       add 1 to table-index
                       add 1 to line-index giving start-index
                   end-if
               end-perform

      *        report is in the table, now process its contents
               perform varying report-index from 1 by 1
                       until report-table(report-index + 1) = 0
                   move report-table(report-index) to value-1
                   move report-table(report-index + 1) to value-2
      d            display value-1 ", " value-2

                   subtract value-1 from value-2 giving diff
      d            display diff
                   if report-index = 1 *> determine starting direction
                       if diff < 0
      d                    display "descending"
                           move "D" to direction
                       else
      d                    display "ascending"
                           move "A" to direction
                       end-if
                   else
                       if (diff < 0 and direction = "A")
                               or (diff > 0 and direction = "D")
      d                    display "direction change"
                           move "N" to safe
                       end-if
                   end-if

                   move function abs(diff) to diff
                   if diff < 1 or diff > 3
      d                display "wrong change: " diff
                       move "N" to safe
                   end-if
               end-perform

               display function trim(input-line) ": " safe
               if safe = "Y"
                   add 1 to total
               end-if
           exit.


