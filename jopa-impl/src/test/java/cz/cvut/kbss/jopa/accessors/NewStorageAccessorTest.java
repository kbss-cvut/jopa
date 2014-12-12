package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Collections;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class NewStorageAccessorTest {

    private static final String DATA_SOURCE_CLASS = "cz.cvut.kbss.jopa.accessors.DataSourceStub";
    private static final String INVALID_DATA_SOURCE_CLASS = "cz.cvut.kbss.jopa.accessors.InvalidDataSource";

    @Mock
    private OntologyStorageProperties storageProperties;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        when(storageProperties.getDataSource()).thenReturn(DATA_SOURCE_CLASS);
    }

    @Test
    public void createsDataSourceFromClassName() throws Exception {
        final NewStorageAccessor a = new NewStorageAccessor(storageProperties, Collections.<String, String>emptyMap());
        assertNotNull(a);
        assertTrue(a.isOpen());
        assertTrue(a.acquireConnection() instanceof DataSourceStub.ConnectionStub);
    }

    @Test(expected = DataSourceCreationException.class)
    public void throwsExceptionWhenDataSourceClassWithoutProperConstructorIsProvided() throws Exception {
        when(storageProperties.getDataSource()).thenReturn(INVALID_DATA_SOURCE_CLASS);
        final NewStorageAccessor a = new NewStorageAccessor(storageProperties, Collections.<String, String>emptyMap());
        // This shouldn't be reached
        assertFalse(a.isOpen());
    }

    @Test
    public void testClose() throws Exception {
        final NewStorageAccessor a = new NewStorageAccessor(storageProperties, Collections.<String, String>emptyMap());
        assertTrue(a.isOpen());
        a.close();
        assertFalse(a.isOpen());
    }
}