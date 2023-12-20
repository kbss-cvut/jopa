# JOPA TODOs 

## High Priority

- [ ] Allow static (annotation, attribute of `@OWLDataProperty`) specification of language of String attributes. 
        This can be overridden on EM operation level (in descriptor).


## Low Priority

- [ ] How to enhance query results with transactional changes? RDF4J driver
    - First check how SQL queries in JPA behave
- [ ] Add possibility to generate integrity constraints from the object model

## Research Topics

- [ ] Data integrity violation checks. E.g. when an entity is removed, there should be a check whether it is referenced from another entity
