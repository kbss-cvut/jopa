package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassA_;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassB_;
import cz.cvut.kbss.jopa.environment.OWLClassC_;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

class StaticMetamodelInitializerTest {

    @Mock
    private Metamodel metamodel;

    private MetamodelMocks metamodelMocks;

    private StaticMetamodelInitializer sut;

    @BeforeEach
    void setUp() throws Exception{
        MockitoAnnotations.openMocks(this);
        this.metamodelMocks = new MetamodelMocks();
        metamodelMocks.setMocks(metamodel);
        this.sut = new StaticMetamodelInitializer(metamodel);
    }

    @Test
    void initializeStaticMetamodelInitializesFieldsOfSimpleEntityStaticMetamodel() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassA().entityType()));
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassA().identifier(), OWLClassA_.uri);
        assertEquals(metamodelMocks.forOwlClassA().stringAttribute(), OWLClassA_.stringAttribute);
        assertEquals(metamodelMocks.forOwlClassA().typesSpec(), OWLClassA_.types);
    }

    @Test
    void initializeStaticMetamodelInitializesPropertiesFieldInStaticMetamodelClass() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassB().entityType()));
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassB().identifier(), OWLClassB_.uri);
        assertEquals(metamodelMocks.forOwlClassB().stringAttribute(), OWLClassB_.stringAttribute);
        assertEquals(metamodelMocks.forOwlClassB().propertiesSpec(), OWLClassB_.properties);
    }

    @Test
    void initializeStaticMetamodelInitializesPluralAttributesInStaticMetamodelClass() {
        when(metamodel.getEntities()).thenReturn(Collections.singleton(metamodelMocks.forOwlClassC().entityType()));
        sut.initializeStaticMetamodel();
        assertEquals(metamodelMocks.forOwlClassC().identifier(), OWLClassC_.uri);
        assertEquals(metamodelMocks.forOwlClassC().referencedListAtt(), OWLClassC_.referencedList);
        assertEquals(metamodelMocks.forOwlClassC().simpleListAtt(), OWLClassC_.simpleList);
    }
}