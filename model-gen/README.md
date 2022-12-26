# model-gen - Static model generator for JOPA

This is bachelors thesis work in progress

As-is:
Right now, generator creates mirror files of Classes annotated with `@MappedSuperclass` or `@OWLClass`.

## To start generator:

The generator need to be defined in maven-compiler-plugin of project which you want to generate model for.
The configuration must be following:

```xml
<configuration>
    <annotationProcessors>
        <annotationProcessor>
            cz.cvut.kbss.jopa.modelgen.AnnotationProcessor
        </annotationProcessor>
    </annotationProcessors>
    <annotationProcessorPaths>
        <annotationProcessorPath>
            <groupId>
                cz.cvut.kbss.jopa
            </groupId>
            <artifactId>model-gen</artifactId>
            <version>${project.parent.version}</version>
        </annotationProcessorPath>
    </annotationProcessorPaths>
</configuration>
```

