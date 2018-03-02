package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.jena.connector.StorageConnector;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Resource;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;
import static org.apache.jena.rdf.model.ResourceFactory.createResource;
import static org.apache.jena.rdf.model.ResourceFactory.createStatement;
import static org.mockito.Mockito.when;

public class ListTestUtil {

    static List<URI> generateList(Resource owner, Property hasList, Property hasNext, StorageConnector connectorMock) {
        final List<URI> list = new ArrayList<>();
        URI previous = null;
        for (int i = 0; i < 5; i++) {
            final URI node = Generator.generateUri();
            list.add(node);
            if (previous != null) {
                final Resource prevResource = createResource(previous.toString());
                when(connectorMock.find(prevResource, hasNext, null)).thenReturn(Collections
                        .singletonList(createStatement(prevResource, hasNext, createResource(node.toString()))));
            }
            previous = node;
        }
        when(connectorMock.find(owner, hasList, null)).thenReturn(
                Collections.singletonList(createStatement(owner, hasList, createResource(list.get(0).toString()))));
        return list;
    }
}
