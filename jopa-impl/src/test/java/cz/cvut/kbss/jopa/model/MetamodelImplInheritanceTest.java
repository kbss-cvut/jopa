/**
 * Copyright (C) 2022 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.loaders.PersistenceUnitClassFinder;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.Constants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class MetamodelImplInheritanceTest {

    private static final Map<String, String> PROPERTIES = Collections
            .singletonMap(JOPAPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa");

    @Mock
    private PersistenceUnitClassFinder classFinderMock;

    private final Configuration conf = new Configuration(PROPERTIES);

    @Test
    void buildsEntityTypeForClassWithMappedSuperclass() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassQ.class);
        final EntityType<OWLClassQ> et = metamodel.entity(OWLClassQ.class);
        assertNotNull(et);
        assertEquals(OWLClassQ.getClassIri(), et.getIRI().toString());
    }

    private MetamodelImpl metamodelFor(Class<?>... classes) {
        when(classFinderMock.getEntities()).thenReturn(new HashSet<>(Arrays.asList(classes)));
        final MetamodelImpl metamodel = new MetamodelImpl(conf);
        metamodel.build(classFinderMock);
        return metamodel;
    }

    @Test
    void entityWithMappedSuperclassIsBuiltWithAllRelevantFields() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassQ.class);
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

    @Test
    void entityWithMappedSuperclassSetsEntityTypeSupertype() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassQ.class);
        final IdentifiableEntityType<OWLClassQ> et = metamodel.entity(OWLClassQ.class);

        final List<? extends IdentifiableType<? super OWLClassQ>> supertype = new LinkedList<>(et.getSupertypes());

        assertEquals(1, supertype.size());
        assertEquals(QMappedSuperclass.class, supertype.iterator().next().getJavaType());
    }

    @Test
    void entityWithAbstractEntityParentSetsEntityTypeSupertype() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, OWLClassS.class);

        final IdentifiableEntityType<OWLClassR> etR = metamodel.entity(OWLClassR.class);
        assertNotNull(etR);
        final IdentifiableEntityType<OWLClassS> etS = metamodel.entity(OWLClassS.class);
        assertNotNull(etS);
        assertEquals(1, etR.getSupertypes().size());
        assertTrue(etR.getSupertypes().contains(etS));
    }

    @Test
    void entityWithEntitySuperclassIsBuiltWithAllRelevantAttributes() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, OWLClassS.class);
        final List<Field> fields = Arrays
                .asList(OWLClassS.getNameField(), OWLClassR.getOwlClassAField(), OWLClassR.getStringAttField());
        final EntityType<OWLClassR> et = metamodel.entity(OWLClassR.class);
        for (Field f : fields) {
            final Attribute<? super OWLClassR, ?> att = et.getAttribute(f.getName());
            assertNotNull(att);
            assertEquals(f, att.getJavaField());
        }
        assertNotNull(et.getIdentifier());
        assertEquals(OWLClassS.getUriField(), et.getIdentifier().getJavaField());
    }

    @Test
    void buildingMetamodelSetsSubtypesOfMappedSuperclass() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassQ.class);
        final Set<AbstractIdentifiableType<? super OWLClassQ>> supertypes = metamodel.entity(OWLClassQ.class).getSupertypes();

        assertEquals(1, supertypes.size());
        final AbstractIdentifiableType<? super OWLClassQ> mappedSupertype = supertypes.iterator().next();

        assertNotNull(mappedSupertype);

        assertTrue(mappedSupertype.hasSubtypes());
        assertTrue(mappedSupertype.getSubtypes().contains(metamodel.entity(OWLClassQ.class)));
    }

    @Test
    void buildingMetamodelSetsSubtypesOfEntitySuperclass() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, OWLClassS.class);
        final IdentifiableEntityType<OWLClassS> supertype = metamodel.entity(OWLClassS.class);
        assertTrue(supertype.hasSubtypes());
        assertTrue(supertype.getSubtypes().contains(metamodel.entity(OWLClassR.class)));
    }

    @Test
    void subtypesOfEntityWithoutSubtypesAreEmpty() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassA.class);
        final IdentifiableEntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        assertFalse(et.hasSubtypes());
        assertTrue(et.getSubtypes().isEmpty());
    }

    @Test
    void buildingMetamodelResolvesInheritanceStrategy() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, OWLClassS.class);
        final Inheritance inheritance = OWLClassS.class.getDeclaredAnnotation(Inheritance.class);
        final IdentifiableEntityType<OWLClassS> sEntityType = metamodel.entity(OWLClassS.class);
        assertEquals(inheritance.strategy(), sEntityType.getInheritanceType());
        final IdentifiableEntityType<OWLClassR> rEntityType = metamodel.entity(OWLClassR.class);
        assertEquals(inheritance.strategy(), rEntityType.getInheritanceType());
    }

    @Test
    void buildingMetamodelSetsDefaultInheritanceStrategyWhenItIsNotSpecifiedOnEntity() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassA.class);
        final IdentifiableEntityType<OWLClassA> et = metamodel.entity(OWLClassA.class);
        assertEquals(Constants.DEFAULT_INHERITANCE_TYPE, et.getInheritanceType());
    }

    @Test
    void buildingMetamodelThrowsExceptionWhenInheritanceStrategyIsDeclaredOnSubtype() {
        final MetamodelInitializationException ex = assertThrows(MetamodelInitializationException.class,
                () -> metamodelFor(OWLClassS.class, SubclassWithInheritanceType.class));
        assertEquals("Class " + SubclassWithInheritanceType.class +
                        " cannot declare inheritance strategy, because it already inherits it from its supertype.",
                ex.getMessage());
    }

    @Inheritance(strategy = InheritanceType.TRY_FIRST)
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#SubclassWithInheritanceType")
    private static final class SubclassWithInheritanceType extends OWLClassS {

    }

    @Test
    void buildingMetamodelSetsMultipleSubtypesOnSuperType() {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, AnotherSubclass.class, OWLClassS.class);
        final IdentifiableEntityType<OWLClassS> supertype = metamodel.entity(OWLClassS.class);
        assertEquals(2, supertype.getSubtypes().size());
        assertTrue(supertype.getSubtypes().contains(metamodel.entity(OWLClassR.class)));
        assertTrue(supertype.getSubtypes().contains(metamodel.entity(AnotherSubclass.class)));
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "AnotherSubtype")
    private static class AnotherSubclass extends OWLClassS {
    }

    /**
     * Ref.: https://github.com/kbss-cvut/jopa/issues/3
     */
    @Test
    void buildingMetamodelSupportsReferenceFromParentEntityToSubEntity() {
        final MetamodelImpl metamodel = metamodelFor(AnotherChildWithCircular.class);
        final EntityType<ChildWithCircular> et = metamodel.entity(ChildWithCircular.class);
        assertNotNull(et);
        assertNotNull(et.getIdentifier());
    }

    @MappedSuperclass
    private static class ParentWithId {
        @Id
        private URI id;
    }

    @MappedSuperclass
    private static class ParentWithCircular extends ParentWithId {

        @OWLObjectProperty(iri = Vocabulary.ATTRIBUTE_BASE + "hasChild")
        private Set<ChildWithCircular> children;
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "ChildWithCircular")
    private static class ChildWithCircular extends ParentWithCircular {
    }

    @OWLClass(iri = Vocabulary.CLASS_BASE + "AnotherChildWithCircular")
    private static class AnotherChildWithCircular extends ParentWithCircular {
    }
}
