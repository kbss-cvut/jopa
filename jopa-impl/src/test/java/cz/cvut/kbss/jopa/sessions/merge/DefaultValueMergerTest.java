package cz.cvut.kbss.jopa.sessions.merge;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.sessions.change.ChangeRecordImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;

@MockitoSettings(strictness = Strictness.LENIENT)
class DefaultValueMergerTest {

    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor;

    private DefaultValueMerger sut;

    @BeforeEach
    void setUp() throws Exception {
        this.metamodelMocks = new MetamodelMocks();
        this.descriptor = new EntityDescriptor();

        this.sut = new DefaultValueMerger();
    }

    @Test
    void mergeValueSetsValueFromChangeRecordDirectlyOnTargetObject() {
        final OWLClassA target = Generators.generateOwlClassAInstance();
        final String newValue = "new test value";
        sut.mergeValue(target, new ChangeRecordImpl(metamodelMocks.forOwlClassA().stringAttribute(), newValue), descriptor);
        assertEquals(newValue, target.getStringAttribute());
    }
}
