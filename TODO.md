# JOPA TODOs 

## High Priority

- [ ] Add support for `orphanRemoval` attribute in object properties
- [ ] Allow static (annotation, attribute of `@OWLDataProperty`) specification of language of String attributes. 
        This can be overridden on EM operation level (in descriptor).


## Low Priority

- [ ] How to enhance query results with transactional changes? RDF4J driver
    - First check how SQL queries in JPA behave
- [ ] Add possibility to generate integrity constraints from the object model
- [ ] When OWL2Java generates classes and they already exist, rewrite only the fields and getters and setters, keep any other code intact
    - CodeModel API does not support any such feature, it always removes files and replaces them with new ones

## Research Topics

- [ ] Data integrity violation checks. E.g. when an entity is removed, there should be a check whether it is referenced from another entity
