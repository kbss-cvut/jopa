package cz.cvut.kbss.jopa.test.integration;

import cz.cvut.kbss.jopa.CommonVocabulary;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.test.OWLClassR;
import cz.cvut.kbss.jopa.test.environment.Generators;
import cz.cvut.kbss.ontodriver.descriptor.AxiomDescriptor;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Test;

import java.net.URI;
import java.util.Arrays;
import java.util.Collection;

import static org.junit.Assert.assertNotEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.when;

/**
 * Test for discovered bugs and their fixes.
 */
public class BugTest extends IntegrationTestBase {

    /* Bug: using an attribute in hashCode/equals caused an infinite loop, because the BeanListenerAspect tried
     to check whether it was necessary to load the field, which caused instance lookup to use hashCode again
     */
    @Test
    public void hashCodeWithAttributeDoesNotCauseInfiniteLoop() throws Exception {
        final URI uri = Generators.generateUri();
        final String name = "Instance1";
        when(connectionMock.find(any(AxiomDescriptor.class))).thenReturn(initAxiomsForR(uri, name));

        final OWLClassR r = em.find(OWLClassR.class, uri);
        assertNotEquals(0, r.hashCode());
    }

    private Collection<Axiom<?>> initAxiomsForR(URI uri, String name) {
        final NamedResource nr = NamedResource.create(uri);
        final String typeIri = OWLClassR.class.getDeclaredAnnotation(OWLClass.class).iri();
        return Arrays.asList(new AxiomImpl<>(nr, Assertion.createClassAssertion(false),
                        new Value<>(NamedResource.create(typeIri))),
                new AxiomImpl<>(nr,
                        Assertion.createAnnotationPropertyAssertion(URI.create(CommonVocabulary.RDFS_LABEL), false),
                        new Value<>(name)));
    }
}
