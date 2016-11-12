package cz.cvut.kbss.jopa.oom.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.AbstractIdentifiableType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.EntityTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.oom.exceptions.AmbiguousEntityTypeException;
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

public class PolymorphicEntityTypeResolverTest {

    private static final NamedResource INDIVIDUAL = NamedResource.create(Generators.createIndividualIdentifier());

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Mock
    private MetamodelImpl metamodelMock;

    @Before
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
    }

    @Test
    public void determineActualEntityTypeReturnsRootTypeWhenTypeAxiomsContainItsClass() {
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final Collection<Axiom<URI>> types = getTypeAxioms(etS.getIRI().toString());

        assertEquals(etS, execute(etS, types));
    }

    private <T> EntityType<? extends T> execute(EntityTypeImpl<T> root, Collection<Axiom<URI>> types) {
        return new PolymorphicEntityTypeResolver<>(INDIVIDUAL, root, types).determineActualEntityType();
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

        assertEquals(etR, execute(etS, types));
    }

    @Test
    public void determineActualEntityTypeReturnsNullForNoMatchingEntityType() {
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final Collection<Axiom<URI>> types = getTypeAxioms();

        assertNull(execute(etS, types));
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

        execute(etS, types);
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
        final EntityTypeImpl<OWLClassR> etR = metamodelMock.entity(OWLClassR.class);
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final EntityTypeImpl extraEt = generateEntityTypeSubtree(1, etR);
        final Collection<Axiom<URI>> types = getTypeAxioms(etR.getIRI().toString(), extraEt.getIRI().toString());

        assertEquals(extraEt, execute(etS, types));
    }

    private EntityTypeImpl generateEntityTypeSubtree(int depth, EntityTypeImpl<?> parent) {
        EntityTypeImpl currentParent = parent;
        EntityTypeImpl current = null;
        for (int i = 0; i < depth; i++) {
            final IRI currentIri = IRI.create(Generators.createIndividualIdentifier().toString());
            current = generateEntityType(currentIri);
            if (currentParent.getSubtypes() != null) {
                final Set<AbstractIdentifiableType> subtypes = new HashSet<>(currentParent.getSubtypes());
                subtypes.add(current);
                when(currentParent.getSubtypes()).thenReturn(subtypes);
            } else {
                when(currentParent.getSubtypes()).thenReturn(Collections.singleton(current));
            }
            when(current.getSupertype()).thenReturn(currentParent);
            currentParent = current;
        }
        return current;
    }

    @Test
    public void determineActualEntityTypeReturnsMostConcreteEtWhenAncestorIsAlsoInTypes() {
        final EntityTypeImpl<OWLClassR> etR = metamodelMock.entity(OWLClassR.class);
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final EntityTypeImpl mostSpecificEt = generateEntityTypeSubtree(Generators.randomPositiveInt(5), etR);
        final Collection<Axiom<URI>> types = getTypeAxioms(etR.getIRI().toString(), mostSpecificEt.getIRI().toString());

        assertEquals(mostSpecificEt, execute(etS, types));
    }

    @Test
    public void determineActualEntityTypeThrowsAmbiguousEtExceptionWhenMultipleEtsInDifferentSubtreesAreFound() {
        final EntityTypeImpl<OWLClassR> etR = metamodelMock.entity(OWLClassR.class);
        final EntityTypeImpl<OWLClassS> etS = metamodelMock.entity(OWLClassS.class);
        final EntityTypeImpl mostSpecificEtOne = generateEntityTypeSubtree(Generators.randomPositiveInt(5), etR);
        final EntityTypeImpl mostSpecificEtTwo = generateEntityTypeSubtree(Generators.randomPositiveInt(5), etR);
        final Collection<Axiom<URI>> types =
                getTypeAxioms(mostSpecificEtOne.getIRI().toString(), mostSpecificEtTwo.getIRI().toString());

        thrown.expect(AmbiguousEntityTypeException.class);
        thrown.expectMessage("Unable to determine unique entity type for loading individual " + INDIVIDUAL +
                ". Matching types are ");
        thrown.expectMessage(mostSpecificEtOne.toString());
        thrown.expectMessage(mostSpecificEtTwo.toString());

        execute(etS, types);
    }

    // TODO Check for entity type being abstract - what should actually happen if the class is abstract?
    // TODO Add also some tests for mapped superclasses - they should be skipped in the traversal
}