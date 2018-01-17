##### Storage connector

- Can have multiple isolation levels
    - Basic reuses shared model for reading, changes are kept in transactional change logs (like in SesameDriver) - added
    and removed statements. This would mean that non-repeatable reads could occur.
    - Snapshot-based - each transaction gets a snapshot of the model.