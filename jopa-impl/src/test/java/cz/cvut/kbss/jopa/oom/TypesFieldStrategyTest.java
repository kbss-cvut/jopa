/**
 * Copyright (C) 2020 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.utils.Generators;
import cz.cvut.kbss.jopa.environment.utils.MetamodelMocks;
import cz.cvut.kbss.jopa.model.descriptors.EntityDescriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import cz.cvut.kbss.ontodriver.model.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

public class TypesFieldStrategyTest {

    private static final URI IDENTIFIER = Generators.createIndividualIdentifier();

    @Mock
    private EntityMappingHelper mapperMock;

    private AxiomValueGatherer gatherer;

    private MetamodelMocks mocks;

    private OWLClassA entityA;

    @BeforeEach
    public void setUp() throws Exception {
        MockitoAnnotations.initMocks(this);

        this.mocks = new MetamodelMocks();
        this.entityA = new OWLClassA();
        entityA.setUri(IDENTIFIER);


        this.gatherer = new AxiomValueGatherer(NamedResource.create(IDENTIFIER), null);
        gatherer.addValue(Assertion.createClassAssertion(false), new Value<>(IDENTIFIER), null);
    }

    private <T> TypesFieldStrategy<T> strategy(EntityType<T> et, TypesSpecification<T, ?> typesSpec) {
        EntityDescriptor descriptor = new EntityDescriptor();
        return new TypesFieldStrategy<>(et, typesSpec,
                descriptor.getAttributeDescriptor(typesSpec), mapperMock);
    }

    @Test
    public void extractsTypesForPersist() throws Exception {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        final int count = 5;
        entityA.setTypes(Generators.generateTypes(count));
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(null);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toAdd = OOMTestUtils.getTypesToAdd(gatherer);
        assertEquals(count, toAdd.size());
        verifyCollectionsAreEqual(entityA.getTypes(), toAdd);
        assertNull(OOMTestUtils.getTypesToRemove(gatherer));
    }

    @Test
    public void extractsAddedTypes() throws Exception {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        final int count = 5;
        entityA.setTypes(Generators.generateTypes(count));
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        final Set<String> addedTypes = new HashSet<>();
        addedTypes.add("http://krizik.felk.cvut.cz/ontologies/jopa#addedOne");
        addedTypes.add("http://krizik.felk.cvut.cz/ontologies/jopa#addedTwo");
        entityA.getTypes().addAll(addedTypes);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toAdd = OOMTestUtils.getTypesToAdd(gatherer);
        assertEquals(addedTypes.size(), toAdd.size());
        verifyCollectionsAreEqual(addedTypes, toAdd);
        assertTrue(OOMTestUtils.getTypesToRemove(gatherer).isEmpty());
    }

    private void verifyCollectionsAreEqual(Set<String> expected, Set<URI> actual) {
        for (URI type : actual) {
            assertTrue(expected.contains(type.toString()));
        }
    }

    private OWLClassA createOriginal() {
        final OWLClassA a = new OWLClassA();
        a.setUri(IDENTIFIER);
        if (entityA.getTypes() != null) {
            a.setTypes(new HashSet<>(entityA.getTypes()));
        }
        a.setStringAttribute(a.getStringAttribute());
        return a;
    }

    @Test
    public void extractsRemovedTypes() throws Exception {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        final int count = 5;
        entityA.setTypes(Generators.generateTypes(count));
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        final Set<String> removedTypes = removeRandomTypes();
        assertFalse(removedTypes.isEmpty());
        assertFalse(entityA.getTypes().isEmpty());

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toRemove = OOMTestUtils.getTypesToRemove(gatherer);
        assertFalse(toRemove.isEmpty());
        verifyCollectionsAreEqual(removedTypes, toRemove);
        assertTrue(OOMTestUtils.getTypesToAdd(gatherer).isEmpty());
    }

    private Set<String> removeRandomTypes() {
        final Set<String> removedTypes = new HashSet<>();
        int i = 0;
        final Iterator<String> it = entityA.getTypes().iterator();
        while (it.hasNext()) {
            if (i % 2 != 0) {
                removedTypes.add(it.next());
                it.remove();
            } else {
                it.next();
            }
            i++;
        }
        return removedTypes;
    }

    @Test
    public void extractsRemovedTypesWhenValueIsNull() throws Exception {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        final int count = 5;
        entityA.setTypes(Generators.generateTypes(count));
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        entityA.setTypes(null);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toRemove = OOMTestUtils.getTypesToRemove(gatherer);
        assertFalse(toRemove.isEmpty());
        verifyCollectionsAreEqual(original.getTypes(), toRemove);
        assertNull(OOMTestUtils.getTypesToAdd(gatherer));
    }

    @Test
    public void extractsAddedAndRemovedTypes() throws Exception {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        final int count = 5;
        entityA.setTypes(Generators.generateTypes(count));
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        final Set<String> removedTypes = removeRandomTypes();
        final Set<String> addedTypes = new HashSet<>();
        addedTypes.add("http://krizik.felk.cvut.cz/ontologies/jopa#addedOne");
        addedTypes.add("http://krizik.felk.cvut.cz/ontologies/jopa#addedTwo");
        entityA.getTypes().addAll(addedTypes);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toRemove = OOMTestUtils.getTypesToRemove(gatherer);
        assertFalse(toRemove.isEmpty());
        verifyCollectionsAreEqual(removedTypes, toRemove);
        final Set<URI> toAdd = OOMTestUtils.getTypesToAdd(gatherer);
        assertEquals(addedTypes.size(), toAdd.size());
        verifyCollectionsAreEqual(addedTypes, toAdd);
    }

    @Test
    public void extractsNothingWhenThereAreNoTypesAndNoOriginal() throws Exception {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(null);
        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        assertNull(OOMTestUtils.getTypesToRemove(gatherer));
        assertNull(OOMTestUtils.getTypesToAdd(gatherer));
    }

    @Test
    public void extractsNothingWhenOriginalTypesAndCurrentTypesAreNull() throws Exception {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        final OWLClassA original = createOriginal();
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);

        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        assertNull(OOMTestUtils.getTypesToRemove(gatherer));
        assertNull(OOMTestUtils.getTypesToAdd(gatherer));
    }

    @Test
    public void extractsTypesToAddWhenOriginalTypesAreNull() throws Exception {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        final int count = 5;
        entityA.setTypes(Generators.generateTypes(count));
        final OWLClassA original = createOriginal();
        original.setTypes(null);
        when(mapperMock.getOriginalInstance(entityA)).thenReturn(original);
        strategy.buildAxiomValuesFromInstance(entityA, gatherer);

        final Set<URI> toAdd = OOMTestUtils.getTypesToAdd(gatherer);
        assertEquals(count, toAdd.size());
    }

    @Test
    public void extractsTypesForPersistFromUriTypes() throws Exception {
        final TypesFieldStrategy<OWLClassP> strategy =
                strategy(mocks.forOwlClassP().entityType(), mocks.forOwlClassP().types());
        final OWLClassP p = new OWLClassP();
        p.setTypes(Generators.generateUriTypes(Generators.DEFAULT_SIZE));

        strategy.buildAxiomValuesFromInstance(p, gatherer);
        final Set<URI> toPersist = OOMTestUtils.getTypesToAdd(gatherer);
        assertEquals(p.getTypes(), toPersist);
    }

    @Test
    public void buildsStringBasedTypesFieldValueFromAxioms() {
        final TypesFieldStrategy<OWLClassA> strategy =
                strategy(mocks.forOwlClassA().entityType(), mocks.forOwlClassA().typesSpec());
        final List<Axiom<URI>> axioms = generateClassAssertionAxioms(OWLClassA.getClassIri());
        axioms.forEach(strategy::addValueFromAxiom);

        final OWLClassA a = new OWLClassA();
        strategy.buildInstanceFieldValue(a);
        final URI classUri = URI.create(OWLClassA.getClassIri());
        assertEquals(axioms.size() - 1, a.getTypes().size());
        axioms.stream().filter(ax -> !ax.getValue().getValue().equals(classUri))
              .forEach(ax -> assertTrue(a.getTypes().contains(ax.getValue().stringValue())));
    }

    private List<Axiom<URI>> generateClassAssertionAxioms(String javaClassIri) {
        final List<Axiom<URI>> axioms = new ArrayList<>();
        final NamedResource subject = NamedResource.create(IDENTIFIER);
        axioms.add(
                new AxiomImpl<>(subject, Assertion.createClassAssertion(false), new Value<>(URI.create(javaClassIri))));
        final Set<URI> types = Generators.generateUriTypes(Generators.DEFAULT_SIZE);
        types.forEach(u -> axioms.add(new AxiomImpl<>(subject, Assertion.createClassAssertion(false), new Value<>(u))));
        return axioms;
    }

    @Test
    public void buildsUriBasedTypesFieldValueFromAxioms() {
        final TypesFieldStrategy<OWLClassP> strategy =
                strategy(mocks.forOwlClassP().entityType(), mocks.forOwlClassP().types());
        final List<Axiom<URI>> axioms = generateClassAssertionAxioms(OWLClassP.getClassIri());
        axioms.forEach(strategy::addValueFromAxiom);

        final OWLClassP p = new OWLClassP();
        strategy.buildInstanceFieldValue(p);
        final URI classUri = URI.create(OWLClassP.getClassIri());
        assertEquals(axioms.size() - 1, p.getTypes().size());
        axioms.stream().filter(ax -> !ax.getValue().getValue().equals(classUri))
              .forEach(ax -> assertTrue(p.getTypes().contains(ax.getValue().getValue())));
    }

    @Test
    public void buildInstanceFieldLeavesFieldNullWhenNoAxiomsAreLoaded() {
        final TypesFieldStrategy<OWLClassP> strategy =
                strategy(mocks.forOwlClassP().entityType(), mocks.forOwlClassP().types());
        final List<Axiom<URI>> axioms = Collections.singletonList(
                new AxiomImpl<>(NamedResource.create(IDENTIFIER), Assertion.createClassAssertion(false),
                        new Value<>(URI.create(OWLClassP.getClassIri()))));
        axioms.forEach(strategy::addValueFromAxiom);

        final OWLClassP p = new OWLClassP();
        assertNull(p.getTypes());
        strategy.buildInstanceFieldValue(p);
        assertNull(p.getTypes());
    }
}
