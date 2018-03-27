package cz.cvut.kbss.ontodriver.jena.list;

import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptorImpl;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.jena.environment.Generator;
import cz.cvut.kbss.ontodriver.model.Assertion;
import org.apache.jena.rdf.model.Property;
import org.apache.jena.rdf.model.Statement;
import org.junit.Before;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.List;

import static org.apache.jena.rdf.model.ResourceFactory.createProperty;

public class ReferencedListHandlerTest
        extends ListHandlerTestBase<ReferencedListDescriptor, ReferencedListValueDescriptor> {

    private static final Assertion HAS_CONTENT = Assertion
            .createObjectPropertyAssertion(Generator.generateUri(), false);
    private static final Property HAS_CONTENT_PROPERTY = createProperty(HAS_CONTENT.getIdentifier().toString());

    @Before
    public void setUp() {
        MockitoAnnotations.initMocks(this);
        this.handler = new ReferencedListHandler(connectorMock);
        super.setUp();
        listUtil.setHasContent(HAS_CONTENT_PROPERTY);
    }

    @Override
    List<URI> generateList(String context) {
        if (context != null) {
            return listUtil.generateReferencedList(context);
        } else {
            return listUtil.generateReferencedList();
        }
    }

    @Override
    ReferencedListDescriptor listDescriptor() {
        return new ReferencedListDescriptorImpl(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
    }

    @Override
    ReferencedListValueDescriptor listValueDescriptor() {
        return new ReferencedListValueDescriptor(OWNER, HAS_LIST, HAS_NEXT, HAS_CONTENT);
    }

    @Override
    List<Statement> getExpectedStatementsForPersist(List<URI> list) {
        return null;
    }
}