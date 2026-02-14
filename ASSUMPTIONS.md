# Assumptions

- Row `n` (1-indexed) must include exactly `n` numbers; otherwise the program exits with an error message.
- All numbers fit inside a signed 64-bit integer; sums are stored in `Long` to avoid overflow for up to 500 rows.
- The solver deliberately interleaves validation with the main DP logic to avoid extra passes over large inputs. That optimization trades readability and flexibility - adding a new check (e.g., for empty rows) would complicate the control flow compared with a separated parser - but was chosen to reduce computation time under the assumption that maintainability and future scalability are not priorities for this assignment.
