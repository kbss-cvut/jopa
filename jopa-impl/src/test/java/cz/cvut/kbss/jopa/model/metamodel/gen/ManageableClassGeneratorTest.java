package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassI;
import cz.cvut.kbss.jopa.environment.OWLClassO;
import cz.cvut.kbss.jopa.environment.Person;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ManageableClassGeneratorTest {

    @Mock
    private UnitOfWorkImpl uow;

    private MetamodelMocks metamodelMocks;

    private final ManageableClassGenerator sut = new ManageableClassGenerator(new Configuration());

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        final MetamodelImpl mm = mock(MetamodelImpl.class);
        metamodelMocks.setMocks(mm);
        when(uow.getMetamodel()).thenReturn(mm);
    }

    @Test
    void generatedClassHasParentTypeAnnotations() {
        final Class<? extends OWLClassA> cls = sut.generate(OWLClassA.class);
        assertNotNull(cls.getAnnotation(OWLClass.class));
        assertEquals(OWLClassA.class.getAnnotation(OWLClass.class).iri(), cls.getAnnotation(OWLClass.class).iri());
    }

    @Test
    void generatedSetterInvokesPersistenceContextAttributeChangeHandler() throws Exception {
        final Class<? extends OWLClassA> cls = sut.generate(OWLClassA.class);

        final OWLClassA instance = cls.getDeclaredConstructor().newInstance();
        assertInstanceOf(Manageable.class, instance);
        assertInstanceOf(OWLClassA.class, instance);
        ((Manageable) instance).setPersistenceContext(uow);
        when(uow.isInTransaction()).thenReturn(true);
        assertEquals(uow, ((Manageable) instance).getPersistenceContext());
        instance.setStringAttribute("test value");
        assertEquals("test value", instance.getStringAttribute());
        verify(uow).attributeChanged(instance, metamodelMocks.forOwlClassA().stringAttribute());
    }

    @Test
    void generatedGetterInvokesPersistenceContextAttributeLoadForLazyAttribute() throws Exception {
        final Class<? extends OWLClassI> cls = sut.generate(OWLClassI.class);

        final OWLClassI instance = cls.getDeclaredConstructor().newInstance();
        assertInstanceOf(Manageable.class, instance);
        assertInstanceOf(OWLClassI.class, instance);
        ((Manageable) instance).setPersistenceContext(uow);
        when(uow.contains(instance)).thenReturn(true);
        assertEquals(uow, ((Manageable) instance).getPersistenceContext());
        assertNull(instance.getOwlClassA());
        verify(uow).loadEntityField(instance, metamodelMocks.forOwlClassI().owlClassAAtt());
    }

    @Test
    void doesOverrideNonGetterMethod() throws Exception {
        final Class<? extends Person> cls = sut.generate(Person.class);

        final Person instance = cls.getDeclaredConstructor().newInstance();
        ((Manageable) instance).setPersistenceContext(uow);
        when(uow.contains(instance)).thenReturn(true);

        assertFalse(instance.isChild());
        verify(uow, never()).loadEntityField(eq(instance), any());
    }

    @Test
    void doesNotOverrideGetterForTransientField() throws Exception {
        final Class<? extends OWLClassO> cls = sut.generate(OWLClassO.class);

        final OWLClassO instance = cls.getDeclaredConstructor().newInstance();
        ((Manageable) instance).setPersistenceContext(uow);
        when(uow.contains(instance)).thenReturn(true);

        assertNull(instance.getTransientField());
        verify(uow, never()).loadEntityField(eq(instance), any());
    }

    @Test
    void doesNotOverrideSetterForTransientField() throws Exception {
        final Class<? extends OWLClassO> cls = sut.generate(OWLClassO.class);

        final OWLClassO instance = cls.getDeclaredConstructor().newInstance();
        ((Manageable) instance).setPersistenceContext(uow);
        when(uow.contains(instance)).thenReturn(true);

        instance.setTransientField("insignificant value");
        verify(uow, never()).attributeChanged(eq(instance), any(FieldSpecification.class));
        verify(uow, never()).attributeChanged(eq(instance), any(Field.class));
    }

    @Test
    void doesNotOverrideIdentifierGetter() throws Exception {
        final Class<? extends OWLClassA> cls = sut.generate(OWLClassA.class);

        final OWLClassA instance = cls.getDeclaredConstructor().newInstance();
        ((Manageable) instance).setPersistenceContext(uow);
        when(uow.contains(instance)).thenReturn(true);
        assertNull(instance.getUri());
        verify(uow, never()).contains(instance);
        verify(uow, never()).loadEntityField(instance, metamodelMocks.forOwlClassA().identifier());
    }
}