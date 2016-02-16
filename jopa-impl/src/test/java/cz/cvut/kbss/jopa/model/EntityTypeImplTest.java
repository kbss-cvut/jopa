package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class EntityTypeImplTest {

    private static Class<OWLClassA> cls;
    private static IRI classIri;
    private static String className;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        cls = OWLClassA.class;
        classIri = IRI.create(OWLClassA.getClassIri());
        className = OWLClassA.class.getName();
    }

    @Test(expected = IllegalArgumentException.class)
    public void getAttributeThrowsIAEWhenAttributeIsNotPresent() throws Exception {
        final EntityType<OWLClassA> et = new EntityTypeImpl<>(className, cls, classIri);
        et.getAttribute("someUnknownAttribute");
    }

    @Test(expected = IllegalArgumentException.class)
    public void getDeclaredAttributeThrowsIAEWhenAttributeIsNotPresent() throws Exception {
        final EntityType<OWLClassA> et = new EntityTypeImpl<>(className, cls, classIri);
        et.getDeclaredAttribute("someUnknownAttribute");
    }

    @Test(expected = IllegalArgumentException.class)
    public void getFieldSpecificationThrowsIAEWhenAttributeIsNotPresent() throws Exception {
        final EntityType<OWLClassA> et = new EntityTypeImpl<>(className, cls, classIri);
        et.getFieldSpecification("someUnknownAttribute");
    }

    @Test
    public void getFieldSpecificationReturnsTypesIfNameMatches() throws Exception {
        final EntityTypeImpl<OWLClassA> et = new EntityTypeImpl<>(className, cls, classIri);
        final TypesSpecification typesSpec = mock(TypesSpecification.class);
        when(typesSpec.getName()).thenReturn(OWLClassA.getTypesField().getName());
        when(typesSpec.getJavaField()).thenReturn(OWLClassA.getTypesField());
        et.addDirectTypes(typesSpec);

        assertEquals(typesSpec, et.getFieldSpecification(typesSpec.getName()));
    }
}