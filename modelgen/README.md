# JOPA Static Metamodel Generator

This is a generator of the static canonical metamodel for JOPA entities.

The generator is a Java _annotation processor_. It is heavily inspired by the [Hibernate JPA Modelgen](https://hibernate.org/orm/tooling/).

Possible configuration parameters are:

| Parameter | Default value | Explanation |
| :-------: |:-------------:| ------------|
| debug     | `false`         | Debug logging. |
| addGenerationDate        | `false`           | Whether to add a comment containing the date of the file generation .|
| addGeneratedAnnotation    | `true`   | Whether to add a `@Generated` annotation containing info about the annotation processor that generated the file.|
