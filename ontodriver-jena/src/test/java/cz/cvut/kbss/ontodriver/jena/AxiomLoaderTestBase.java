package cz.cvut.kbss.ontodriver.jena;

import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import org.apache.jena.rdf.model.Resource;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import static org.apache.jena.rdf.model.ResourceFactory.createResource;

public abstract class AxiomLoaderTestBase {

    static final NamedResource SUBJECT = NamedResource.create(Generator.generateUri());
    static final Resource SUBJECT_RES = createResource(SUBJECT.getIdentifier().toString());
    static final URI CONTEXT = Generator.generateUri();

    Map<String, Assertion> mapAssertions(AxiomDescriptor descriptor) {
        final Map<String, Assertion> map = new HashMap<>();
        descriptor.getAssertions().forEach(a -> map.put(a.getIdentifier().toString(), a));
        return map;
    }
}
