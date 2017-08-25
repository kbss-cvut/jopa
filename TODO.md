# JOPA TODOs 

## High Priority

- [ ] Inheritance support
    - [x] Mapped superclass
    - [x] Single inheritance
    - [ ] Multiple inheritance
- [ ] Benchmark comparison with Empire and AliBaba
- [ ] Add support for IC validation disabling on entity load
    - Some sort of loading modes should be added (similar to Lock modes in JPA)
- [ ] JOPA Specification doc, which would explain behaviour and principles
    - Perhaps use the github wiki? Or something on KBSS Liferay?
- [ ] When persisting a collection of instances without generated ids, how to handle references between them?
    - It can happen that JOPA will try to insert a referenced individual which has not yet set id, which
        results in assigning it a generated URI. E.g. `foreach(inst -> {inst.generateUri(); em.persist(inst); })`    
- [ ] Add support for `EntityManager.getReference`
- [ ] Add a `@Context` annotation, which would specify that a field/entity should always be loaded from the specified context.
    - This could be overwritten with a descriptor passed to EM
- [ ] Add support for `orphanRemoval` attribute in object properties
- [ ] Modify OntoDriver API to support Fetch joins
- [ ] Allow static (annotation, attribute of `@OWLDataProperty`) specification of language of String attributes. 
        This can be overridden on EM operation level (in descriptor).


## Low Priority

- [ ] How to enhance query results with transactional changes? Sesame
    - First check how SQL queries in JPA behave
- [ ] Add possibility to generate integrity constraints from the object model
- [ ] When OWL2Java generates classes and they already exist, rewrite only the fields and getters and setters, keep any other code intact
    - CodeModel API does not support any such feature, it always removes files and replaces them with new ones
- [ ] Add support for Lobs and Blobs    

## Research Topics

- [ ] Data integrity violation checks. E.g. when an entity is removed, there should be a check whether it is referenced from another entity
- [ ] Define schema for persistence.xml for JOPA
- [ ] Research whether we could replace aspectj with cglib-generated proxies

### Currently in Progress

- [ ] Fix pending persist handling:
    - [x] Add property assertion to pending persist when persist/merge should add new instance which is not cascaded
    - [x] Remove property assertion from pending persist and persist it when the target is persisted
    - [x] Remove property assertion from pending persist when the original persist is cancelled
    - [x] Remove property assertion from pending persist when the attribute is set to another value
    - [ ] Handling of simple lists
    - [ ] Handling of reference lists
