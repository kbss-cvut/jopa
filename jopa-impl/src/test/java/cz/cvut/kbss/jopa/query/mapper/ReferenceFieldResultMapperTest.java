package cz.cvut.kbss.jopa.query.mapper;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassD;
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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ReferenceFieldResultMapperTest {

    @Mock
    private ResultSet resultSetMock;

    @Mock
    private UnitOfWork uowMock;

    private String fieldName;

    private ReferenceFieldResultMapper mapper;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final MetamodelMocks mocks = new MetamodelMocks();
        this.fieldName = mocks.forOwlClassD().owlClassAAtt().getName();
        this.mapper = new ReferenceFieldResultMapper(mocks.forOwlClassD().owlClassAAtt());
    }

    @Test
    public void mapLoadsInstanceByIdentifierFromUnitOfWork() throws Exception {
        final URI identifier = Generators.createIndividualIdentifier();
        when(resultSetMock.getObject(fieldName)).thenReturn(identifier);
        final OWLClassA aInstance = Generators.generateOwlClassAInstance();
        aInstance.setUri(identifier);
        when(uowMock.readObject(eq(OWLClassA.class), eq(identifier), any())).thenReturn(aInstance);

        final OWLClassD result = new OWLClassD();
        mapper.map(resultSetMock, result, uowMock);
        assertEquals(aInstance, result.getOwlClassA());
        verify(uowMock).readObject(OWLClassA.class, identifier, new EntityDescriptor());
    }
}