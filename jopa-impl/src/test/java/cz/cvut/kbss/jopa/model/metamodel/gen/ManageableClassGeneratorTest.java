package cz.cvut.kbss.jopa.model.metamodel.gen;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassI;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.Manageable;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class ManageableClassGeneratorTest {

    @Mock
    private UnitOfWorkImpl uow;

    private final ManageableClassGenerator sut = new ManageableClassGenerator();

    @BeforeEach
    void setUp() throws Exception {
        final MetamodelMocks metamodelMocks = new MetamodelMocks();
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
        verify(uow).attributeChanged(instance, OWLClassA.getStrAttField());
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
        verify(uow).loadEntityField(instance, OWLClassI.getOwlClassAField());
    }
}
