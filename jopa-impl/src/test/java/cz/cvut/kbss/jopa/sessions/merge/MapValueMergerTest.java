package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import sun.security.krb5.internal.crypto.Des;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.*;

public class MapValueMergerTest {

    @Mock
    private Metamodel metamodel;

    private FieldSpecification<? super OWLClassB, ?> propertiesSpec;

    private Descriptor descriptor;

    private MapValueMerger merger;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodel);
        this.descriptor = new EntityDescriptor();
        final EntityType<OWLClassB> et = metamodel.entity(OWLClassB.class);
        this.propertiesSpec = et.getProperties();

        this.merger = new MapValueMerger();
    }

    @Test
    public void mergeMergesPropertiesMap() throws Exception {
        final OWLClassB orig = new OWLClassB(Generators.createIndividualIdentifier());
        orig.setProperties(Generators.generateStringProperties());
        final OWLClassB merged = new OWLClassB(orig.getUri());
        merged.setProperties(new HashMap<>(orig.getProperties()));
        final String newKey = Generators.createPropertyIdentifier().toString();
        merged.getProperties().put(newKey, new HashSet<>());
        final String vOne = "valueOne";
        final String vTwo = "valueTwo";
        merged.getProperties().get(newKey).add(vOne);
        merged.getProperties().get(newKey).add(vTwo);

        merger.mergeValue(propertiesSpec, orig, orig.getProperties(), merged.getProperties(), descriptor);
        assertEquals(merged.getProperties(), orig.getProperties());
    }

    @Test
    public void mergeSetsFieldValueToNullWhenMergedInstanceValueIsNull() throws Exception {
        final OWLClassB orig = new OWLClassB(Generators.createIndividualIdentifier());
        orig.setProperties(Generators.generateStringProperties());
        final OWLClassB merged = new OWLClassB(orig.getUri());
        merged.setProperties(null);

        merger.mergeValue(propertiesSpec, orig, orig.getProperties(), merged.getProperties(), descriptor);
        assertNull(orig.getProperties());
    }
}