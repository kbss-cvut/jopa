package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.UnitOfWork;
import cz.cvut.kbss.ontodriver.ResultSet;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;

import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ObjectPropertyFieldResultMapperTest {

    @Mock
    private ResultSet resultSetMock;

    @Mock
    private UnitOfWork uowMock;

    private MetamodelMocks metamodelMocks;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.metamodelMocks = new MetamodelMocks();
    }

    @Test
    public void mapLoadsInstanceByIdentifierFromUnitOfWork() throws Exception {
        final FieldResultMapper mapper = new ObjectPropertyFieldResultMapper(
                metamodelMocks.forOwlClassD().owlClassAAtt());
        final String fieldName = metamodelMocks.forOwlClassD().owlClassAAtt().getName();
        final URI identifier = Generators.createIndividualIdentifier();
        when(resultSetMock.getObject(fieldName)).thenReturn(identifier);
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        aInstance.setUri(identifier);
        when(uowMock.readObject(eq(OWLClassA.class), eq(identifier), any())).thenReturn(aInstance);

        final OWLClassD target = new OWLClassD();
        mapper.map(resultSetMock, target, uowMock);
        assertEquals(aInstance, target.getOwlClassA());
        verify(uowMock).readObject(OWLClassA.class, identifier, new EntityDescriptor());
    }

    @Test
    public void mapUsesValueDirectlyWhenFieldIsPlainIdentifier() throws Exception {
        final FieldResultMapper mapper = new ObjectPropertyFieldResultMapper(
                metamodelMocks.forOwlClassP().pUriAttribute());
        final String fieldName = metamodelMocks.forOwlClassP().pUriAttribute().getName();
        final URI value = Generators.createIndividualIdentifier();
        when(resultSetMock.getObject(fieldName)).thenReturn(value);
        final OWLClassP target = new OWLClassP();

        mapper.map(resultSetMock, target, uowMock);
        assertEquals(value, target.getIndividualUri());
        verify(uowMock, never()).readObject(any(), eq(value), any());
    }
}