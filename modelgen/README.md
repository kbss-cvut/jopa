# modelgen - JOPA Static metamodel generator


## To start generator:
There are two ways fo using this generator
### 1. Using jopa-maven-plugin
Generator is integrated as a goal in jopa-maven-plugin. It has 4 configuration parameters:

'output-directory' defines where will metamodel be generated, default output directory is 'target/generated-sources/static-metamodel'

'additional-sources' defines path to additional sources, such as output files of OWL2Java

'source-package' if user wants tu run generator on specific package, it will be defined here and it is in format 'test.example.classesForModelGen'

'debug-option' controls console output of generator, default value is false

### 2. Using only its annotation processor
The generator need to be defined in maven-compiler-plugin of project which you want to generate model for.
The configuration must be following:

```xml

<configuration>
    <annotationProcessors>
        <annotationProcessor>
            cz.cvut.kbss.jopa.modelgen.ModelGenProcessor
        </annotationProcessor>
    </annotationProcessors>
    <annotationProcessorPaths>
        <annotationProcessorPath>
            <groupId>
                cz.cvut.kbss.jopa
            </groupId>
            <artifactId>modelgen</artifactId>
            <version>${project.parent.version}</version>
        </annotationProcessorPath>
    </annotationProcessorPaths>
</configuration>
```

