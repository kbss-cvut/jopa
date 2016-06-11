package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.when;

public class MetamodelImplInheritanceTest {

    private static final Map<String, String> PROPERTIES = Collections
            .singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa");

    @Mock
    private EntityLoader entityLoaderMock;

    private Configuration conf = new Configuration(PROPERTIES);

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void buildsEntityTypeForClassWithMappedSuperclass() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassQ.class));
        final MetamodelImpl metamodel = new MetamodelImpl(conf, entityLoaderMock);
        final EntityType<OWLClassQ> et = metamodel.entity(OWLClassQ.class);
        assertNotNull(et);
        assertEquals(OWLClassQ.getClassIri(), et.getIRI().toString());
    }

    @Test
    public void entityWithMappedSuperclassIsBuiltWithAllRelevantFields() throws Exception {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(Collections.singleton(OWLClassQ.class));
        final MetamodelImpl metamodel = new MetamodelImpl(conf, entityLoaderMock);
        final EntityType<OWLClassQ> et = metamodel.entity(OWLClassQ.class);

        assertNotNull(et.getIdentifier());
        assertEquals(OWLClassQ.getUriField(), et.getIdentifier().getJavaField());
        final List<Field> fields = Arrays.asList(OWLClassQ.getOwlClassAField(), OWLClassQ.getLabelField(),
                OWLClassQ.getParentStringField(), OWLClassQ.getStringAttributeField());
        for (Field f : fields) {
            assertNotNull(et.getFieldSpecification(f.getName()));
            assertEquals(f, et.getFieldSpecification(f.getName()).getJavaField());
        }
    }
}
