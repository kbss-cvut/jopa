package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import org.junit.Test;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;

import static org.junit.Assert.*;

@SuppressWarnings("unused")
public class EntityFieldSeekerTest {

    private EntityFieldSeeker fieldSeeker = new EntityFieldSeeker();

    @Test
    public void discoverFieldsInEntityClassWithoutParentsFindsAllDeclaredFields() throws Exception {
        final Collection<Field> result = fieldSeeker.discoverFields(OWLClassM.class);
        assertTrue(result.size() >= 7); // Can't use equals, because the AspectJ joint points are also returned
        assertTrue(result.contains(OWLClassM.getUriField()));
        assertTrue(result.contains(OWLClassM.getBooleanAttributeField()));
        assertTrue(result.contains(OWLClassM.getDateAttributeField()));
        assertTrue(result.contains(OWLClassM.getDoubleAttributeField()));
        assertTrue(result.contains(OWLClassM.getEnumAttributeField()));
        assertTrue(result.contains(OWLClassM.getIntAttributeField()));
        assertTrue(result.contains(OWLClassM.getLongAttributeField()));
    }

    @Test
    public void discoverFieldsFindsFieldsInEntityAndMappedSuperclass() throws Exception {
        final Collection<Field> result = fieldSeeker.discoverFields(OWLClassQ.class);
        assertTrue(result.size() >= 5); // Can't use equals, because the AspectJ joint points are also returned
        assertTrue(result.contains(OWLClassQ.getUriField()));
        assertTrue(result.contains(OWLClassQ.getLabelField()));
        assertTrue(result.contains(OWLClassQ.getOwlClassAField()));
        assertTrue(result.contains(OWLClassQ.getParentStringField()));
        assertTrue(result.contains(OWLClassQ.getStringAttributeField()));
    }

    @Test
    public void discoverFieldsIgnoresNonEntitySuperclasses() throws Exception {
        final Collection<Field> result = fieldSeeker.discoverFields(Child.class);
        assertTrue(result.contains(Child.class.getDeclaredField("uri")));
        assertFalse(result.contains(NonEntityParent.class.getDeclaredField("label")));
    }

    private static class NonEntityParent {
        private String label;
    }

    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#ChildWithNonEntityParent")
    private static class Child extends NonEntityParent {
        @Id
        private URI uri;
    }
}