package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.FieldSpecification;
import cz.cvut.kbss.ontodriver.descriptor.AxiomValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Assertion;
import cz.cvut.kbss.ontodriver.model.AxiomImpl;
import cz.cvut.kbss.ontodriver.model.NamedResource;
import cz.cvut.kbss.ontodriver.model.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class SingularObjectPropertyStrategyTest {

    private static final URI PK = Generators.createIndividualIdentifier();
    private static final URI VALUE = URI.create("http://krizik.felk.cvut.cz/ontologies/jopa/individualAAA");

    @Mock
    private EntityMappingHelper mapperMock;

    private MetamodelMocks metamodelMocks;

    private Descriptor descriptor = new EntityDescriptor();

    private AxiomValueGatherer gatherer;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        this.metamodelMocks = new MetamodelMocks();
        this.gatherer = new AxiomValueGatherer(NamedResource.create(PK), null);
    }

    @Test
    public void buildInstanceFieldSupportsPlainIdentifierValues() throws Exception {
        final FieldStrategy<? extends FieldSpecification<? super OWLClassP, ?>, OWLClassP> strategy = new SingularObjectPropertyStrategy<>(
                metamodelMocks.forOwlClassP().entityType(), metamodelMocks.forOwlClassP().pUriAttribute(), descriptor,
                mapperMock);
        strategy.addValueFromAxiom(
                new AxiomImpl<>(NamedResource.create(PK), propertyP(), new Value<>(NamedResource.create(VALUE))));
        final OWLClassP p = new OWLClassP();
        strategy.buildInstanceFieldValue(p);
        assertEquals(VALUE, p.getIndividualUri());
    }

    private Assertion propertyP() throws Exception {
        final URI uri = URI.create(OWLClassP.getIndividualUriField().getAnnotation(OWLObjectProperty.class).iri());
        return Assertion.createObjectPropertyAssertion(uri, false);
    }

    @Test
    public void buildsAxiomFromPlainIdentifierValue() throws Exception {
        final OWLClassP p = new OWLClassP();
        p.setIndividualUri(VALUE);
        final FieldStrategy<? extends FieldSpecification<? super OWLClassP, ?>, OWLClassP> strategy = new SingularObjectPropertyStrategy<>(
                metamodelMocks.forOwlClassP().entityType(), metamodelMocks.forOwlClassP().pUriAttribute(), descriptor,
                mapperMock);
        strategy.buildAxiomValuesFromInstance(p, gatherer);
        final AxiomValueDescriptor valueDescriptor = OOMTestUtils.getAxiomValueDescriptor(gatherer);
        assertEquals(1, valueDescriptor.getAssertions().size());
        final List<Value<?>> axioms = valueDescriptor
                .getAssertionValues(propertyP());
        assertEquals(1, axioms.size());
        assertEquals(NamedResource.create(VALUE), axioms.get(0).getValue());
    }
}