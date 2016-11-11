package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.oom.exceptions.AmbiguousEntityTypeException;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class EntityInstanceLoaderTest {

    private static final NamedResource INDIVIDUAL = NamedResource.create(Generators.createIndividualIdentifier());

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private Connection connectionMock;

    @Mock
    private MetamodelImpl metamodelMock;

    @Mock
    private AxiomDescriptorFactory descriptorFactoryMock;

    @Mock
    private EntityConstructor entityBuilderMock;

    private EntityInstanceLoader instanceLoader;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.instanceLoader = new TwoStepInstanceLoader(connectionMock, metamodelMock, descriptorFactoryMock,
                entityBuilderMock);
    }

    @Test
    public void determineActualEntityTypeReturnsRootTypeWhenTypeAxiomsContainItsClass() {
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final Collection<Axiom<URI>> types = getTypeAxioms(etS.getIRI().toString());

        assertEquals(etS, instanceLoader.determineActualEntityType(INDIVIDUAL, etS, types));
    }

    private static Collection<Axiom<URI>> getTypeAxioms(String... types) {
        final List<String> allTypes = new ArrayList<>(Arrays.asList(types));
        allTypes.addAll(Generators.generateTypes(5));
        return allTypes.stream().map(t -> new AxiomImpl<>(INDIVIDUAL,
                Assertion.createClassAssertion(false), new Value<>(URI.create(t)))).collect(Collectors.toList());
    }

    @Test
    public void determineActualEntityTypeReturnsSubtypeWhoseIriIsPresentIntTypeAxioms() {
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final EntityTypeImpl<OWLClassR> etR = metamodelMock.entity(OWLClassR.class);
        final Collection<Axiom<URI>> types = getTypeAxioms(etR.getIRI().toString());

        assertEquals(etR, instanceLoader.determineActualEntityType(INDIVIDUAL, etS, types));
    }

    @Test
    public void determineActualEntityTypeReturnsNullForNoMatchingEntityType() {
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final Collection<Axiom<URI>> types = getTypeAxioms();

        assertNull(instanceLoader.determineActualEntityType(INDIVIDUAL, etS, types));
    }

    @Test
    public void determineActualEntityTypeThrowsAmbiguousEtExceptionWhenMultipleEntityTypesAreFound() {
        final IRI extraEtIri = IRI.create(Generators.createIndividualIdentifier().toString());
        final EntityTypeImpl extraEt = generateEntityType(extraEtIri);
        final EntityTypeImpl<OWLClassR> etR = metamodelMock.entity(OWLClassR.class);
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final Set subtypes = new HashSet<>(Arrays.asList(etR, extraEt));
        when(etS.getSubtypes()).thenReturn(subtypes);
        final Collection<Axiom<URI>> types = getTypeAxioms(etR.getIRI().toString(), extraEtIri.toString());
        thrown.expect(AmbiguousEntityTypeException.class);
        thrown.expectMessage("Unable to determine unique entity type for loading individual " + INDIVIDUAL +
                ". Matching types are ");
        thrown.expectMessage(etR.toString());
        thrown.expectMessage(extraEt.toString());

        instanceLoader.determineActualEntityType(INDIVIDUAL, metamodelMock.entity(OWLClassS.class), types);
    }

    private EntityTypeImpl generateEntityType(IRI etIri) {
        final EntityTypeImpl extraEt = mock(EntityTypeImpl.class);
        when(extraEt.getPersistenceType()).thenReturn(Type.PersistenceType.ENTITY);
        when(extraEt.getSubtypes()).thenReturn(Collections.emptySet());
        when(extraEt.getIRI()).thenReturn(etIri);
        return extraEt;
    }

    @Test
    public void determineActualEntityTypeReturnsMostConcreteEtWhenParentEtIsAlsoInTypes() {
        final IRI extraEtIri = IRI.create(Generators.createIndividualIdentifier().toString());
        final EntityTypeImpl extraEt = generateEntityType(extraEtIri);
        final EntityTypeImpl<OWLClassR> etR = metamodelMock.entity(OWLClassR.class);
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        when(etR.getSubtypes()).thenReturn(Collections.singleton(extraEt));
        when(extraEt.getSupertype()).thenReturn(etR);
        final Collection<Axiom<URI>> types = getTypeAxioms(etR.getIRI().toString(), extraEtIri.toString());

        assertEquals(extraEt, instanceLoader.determineActualEntityType(INDIVIDUAL, etS, types));
    }
}
