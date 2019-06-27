package cz.cvut.kbss.jopa.model.descriptors;

import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ObjectPropertyCollectionDescriptorTest {

    @Test
    void getAttributeContextRetrievesAttributeContextFromElementDescriptor() throws Exception {
        final URI context = URI.create("http://onto.fel.cvut.cz/ontologies/jopa/test");
        final FieldSpecification fs = mock(FieldSpecification.class);
        when(fs.getJavaField()).thenReturn(TestClass.stringAttField());
        final ObjectPropertyCollectionDescriptor descriptor = new ObjectPropertyCollectionDescriptor(context,
                WithReference.class.getDeclaredField("testClass"));
        assertEquals(context, descriptor.getAttributeContext(fs));
    }

    @SuppressWarnings("unused")
    private static class WithReference {

        @OWLObjectProperty(iri = "http://onto.fel.cvut.cz/ontologies/jopa/test/reference")
        private Set<TestClass> testClass;
    }
}