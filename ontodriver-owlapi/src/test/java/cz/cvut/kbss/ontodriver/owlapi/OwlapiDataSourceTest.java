package cz.cvut.kbss.ontodriver.owlapi;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import org.junit.Test;

import java.io.File;
import java.net.URI;

import static org.junit.Assert.*;

public class OwlapiDataSourceTest {

    private OwlapiDataSource dataSource = new OwlapiDataSource();

    @Test(expected = IllegalStateException.class)
    public void throwsExceptionWhenGetConnectionCalledOnClosed() throws Exception {
        assertTrue(dataSource.isOpen());
        dataSource.close();
        assertFalse(dataSource.isOpen());
        dataSource.getConnection();
    }

    @Test(expected = IllegalStateException.class)
    public void throwsExceptionWhenGetConnectionCalledOnUninitialized() throws Exception {
        assertTrue(dataSource.isOpen());
        dataSource.getConnection();
    }

    @Test
    public void testGetConnection() throws Exception {
        final File output = new File(
                System.getProperty("java.io.tmpdir") + File.separator + "datasource_" + System.currentTimeMillis() +
                        ".owl");
        output.deleteOnExit();
        final OntologyStorageProperties p = OntologyStorageProperties.ontologyUri(URI.create("http://example.com"))
                                                                     .physicalUri(output.toURI())
                                                                     .driver(OwlapiDataSource.class.getCanonicalName())
                                                                     .build();
        dataSource.setStorageProperties(p);
        final Connection connection = dataSource.getConnection();
        assertNotNull(connection);
    }
}