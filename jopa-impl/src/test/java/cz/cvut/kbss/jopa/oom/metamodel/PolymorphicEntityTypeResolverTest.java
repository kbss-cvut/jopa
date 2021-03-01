/**
 * Copyright (C) 2020 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom.metamodel;

import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.oom.exceptions.AmbiguousEntityTypeException;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;
import java.util.stream.Collectors;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class PolymorphicEntityTypeResolverTest {

    private static final NamedResource INDIVIDUAL = NamedResource.create(Generators.createIndividualIdentifier());

    @Mock
    private MetamodelImpl metamodelMock;

    private EntityTypeImpl<OWLClassS> etS;
    private EntityTypeImpl<OWLClassR> etR;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.openMocks(this);
        final MetamodelMocks mocks = new MetamodelMocks();
        mocks.setMocks(metamodelMock);
        this.etS = metamodelMock.entity(OWLClassS.class);
        this.etR = metamodelMock.entity(OWLClassR.class);
    }

    @Test
    public void determineActualEntityTypeReturnsRootTypeWhenTypeAxiomsContainItsClass() {
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
        final Collection<Axiom<URI>> types = getTypeAxioms(etR.getIRI().toString());

        assertEquals(etR, execute(etS, types));
    }

    @Test
    public void determineActualEntityTypeReturnsNullForNoMatchingEntityType() {
        final Collection<Axiom<URI>> types = getTypeAxioms();

        assertNull(execute(etS, types));
    }

    @Test
    public void determineActualEntityTypeThrowsAmbiguousEtExceptionWhenMultipleEntityTypesAreFound() {
        final IRI extraEtIri = IRI.create(Generators.createIndividualIdentifier().toString());
        final EntityTypeImpl extraEt = generateEntityType(extraEtIri);
        final Set<AbstractIdentifiableType<? extends OWLClassS>> subtypes = new HashSet<>(
                Arrays.<AbstractIdentifiableType<? extends OWLClassS>>asList(etR, extraEt));
        when(etS.getSubtypes()).thenReturn(subtypes);
        final Collection<Axiom<URI>> types = getTypeAxioms(etR.getIRI().toString(), extraEtIri.toString());

        final AmbiguousEntityTypeException ex = assertThrows(AmbiguousEntityTypeException.class,
                () -> execute(etS, types));
        assertThat(ex.getMessage(),
                containsString("Unable to determine unique entity type for loading individual " + INDIVIDUAL +
                        ". Matching types are "));
        assertThat(ex.getMessage(), containsString(etR.toString()));
        assertThat(ex.getMessage(), containsString(extraEt.toString()));
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
        final EntityTypeImpl mostSpecificEt = generateEntityTypeSubtree(Generators.randomPositiveInt(5), etR);
        final Collection<Axiom<URI>> types = getTypeAxioms(etR.getIRI().toString(), mostSpecificEt.getIRI().toString());

        assertEquals(mostSpecificEt, execute(etS, types));
    }

    @Test
    public void determineActualEntityTypeThrowsAmbiguousEtExceptionWhenMultipleEtsInDifferentSubtreesAreFound() {
        final EntityTypeImpl mostSpecificEtOne = generateEntityTypeSubtree(Generators.randomPositiveInt(5), etR);
        final EntityTypeImpl mostSpecificEtTwo = generateEntityTypeSubtree(Generators.randomPositiveInt(5), etR);
        final Collection<Axiom<URI>> types =
                getTypeAxioms(mostSpecificEtOne.getIRI().toString(), mostSpecificEtTwo.getIRI().toString());

        final AmbiguousEntityTypeException ex = assertThrows(AmbiguousEntityTypeException.class,
                () -> execute(etS, types));
        assertThat(ex.getMessage(),
                containsString("Unable to determine unique entity type for loading individual " + INDIVIDUAL +
                        ". Matching types are "));
        assertThat(ex.getMessage(), containsString(mostSpecificEtOne.toString()));
        assertThat(ex.getMessage(), containsString(mostSpecificEtTwo.toString()));
    }

    @Test
    public void determineActualEntityTypeTraversesOverMappedSuperclassInEntityHierarchy() {
        final EntityTypeImpl rootEt = generateEntityType(
                IRI.create(Generators.createIndividualIdentifier().toString()));
        final EntityTypeImpl<OWLClassQ> etQ = metamodelMock.entity(OWLClassQ.class);
        final MappedSuperclassType mappedSuperclassType = (MappedSuperclassType) etQ.getSupertype();
        when(mappedSuperclassType.getSupertype()).thenReturn(rootEt);
        when(rootEt.getSubtypes()).thenReturn(Collections.singleton(mappedSuperclassType));
        final Collection<Axiom<URI>> types = getTypeAxioms(etQ.getIRI().toString());

        assertEquals(etQ, execute(rootEt, types));
    }

    @Test
    public void determineActualEntityTypeReturnsNullWhenMostSpecificTypeIsAbstract() {
        final String abstractTypeIri = Generators.createIndividualIdentifier().toString();
        final EntityTypeImpl<AbstractSubtype> etAbstract = spy(
                new EntityTypeImpl<>(AbstractSubtype.class.getSimpleName(),
                        AbstractSubtype.class, IRI.create(abstractTypeIri)));
        doReturn(etR).when(etAbstract).getSupertype();
        doReturn(Collections.singleton(etAbstract)).when(etR).getSubtypes();

        final Collection<Axiom<URI>> types = getTypeAxioms(abstractTypeIri);
        assertNull(execute(etS, types));
    }

    private static abstract class AbstractSubtype extends OWLClassR {
    }

    @Test
    public void determineActualEntityTypeIsNotAmbiguousWhenOneOfTheTypesIsAbstract() {
        final String abstractTypeIri = Generators.createIndividualIdentifier().toString();
        final EntityTypeImpl<AbstractSubtype> etAbstract = spy(
                new EntityTypeImpl<>(AbstractSubtype.class.getSimpleName(),
                        AbstractSubtype.class, IRI.create(abstractTypeIri)));
        doReturn(etR).when(etAbstract).getSupertype();
        doReturn(Collections.singleton(etAbstract)).when(etR).getSubtypes();
        final EntityTypeImpl mostSpecificEtOne = generateEntityTypeSubtree(Generators.randomPositiveInt(5), etR);

        final Collection<Axiom<URI>> types = getTypeAxioms(mostSpecificEtOne.getIRI().toString(), abstractTypeIri);
        assertEquals(mostSpecificEtOne, execute(etS, types));
    }

    @Test
    public void determineActualEntityTypeSearchesForMoreSpecificTypeWhenRootTypeIsAbstract() {
        final String abstractTypeIri = Generators.createIndividualIdentifier().toString();
        final EntityTypeImpl<AbstractSubtype> etAbstract = spy(
                new EntityTypeImpl<>(AbstractSubtype.class.getSimpleName(),
                        AbstractSubtype.class, IRI.create(abstractTypeIri)));
        doReturn(Collections.singleton(etS)).when(etAbstract).getSubtypes();
        // We are abusing the type erasure here a little
        when(etS.getSupertype()).thenReturn((AbstractIdentifiableType) etAbstract);

        final Collection<Axiom<URI>> types = getTypeAxioms(OWLClassS.getClassIri(), abstractTypeIri);
        assertEquals(etS, execute(etAbstract, types));
    }
}
