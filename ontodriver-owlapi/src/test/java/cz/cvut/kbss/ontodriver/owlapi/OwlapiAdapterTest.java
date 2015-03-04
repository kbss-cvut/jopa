package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.owlapi.connector.Connector;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.semanticweb.owlapi.model.OWLOntologyChange;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

import static org.mockito.Mockito.*;

public class OwlapiAdapterTest {

    @Mock
    private Connector connectorMock;

    private OwlapiAdapter adapter;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        this.adapter = new OwlapiAdapter(connectorMock);
    }

    @Test
    public void commitSendsChangesToConnector() throws Exception {
        startTransaction();
        generateChange();
        adapter.commit();
        verify(connectorMock).applyChanges(any());
    }

    private void generateChange() throws Exception {
        final Field changesField = OwlapiAdapter.class.getDeclaredField("pendingChanges");
        changesField.setAccessible(true);
        final List<OWLOntologyChange> changes = (List<OWLOntologyChange>) changesField.get(adapter);
        changes.add(mock(OWLOntologyChange.class));
    }

    private void startTransaction() throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Method startTransaction = OwlapiAdapter.class.getDeclaredMethod("startTransactionIfNotActive");
        startTransaction.setAccessible(true);
        startTransaction.invoke(adapter);
    }

    @Test
    public void rollbackCausesChangesToEmpty() throws Exception {
        startTransaction();
        generateChange();
        adapter.rollback();
        adapter.commit();
        verify(connectorMock, never()).applyChanges(any());
    }
}