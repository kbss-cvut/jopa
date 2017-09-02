/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.exception.MetamodelInitializationException;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.annotations.*;
import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.utils.Configuration;
import cz.cvut.kbss.jopa.utils.Constants;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.Mockito.when;

public class MetamodelImplInheritanceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

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
        final MetamodelImpl metamodel = metamodelFor(OWLClassQ.class);
        final EntityType<OWLClassQ> et = metamodel.entity(OWLClassQ.class);
        assertNotNull(et);
        assertEquals(OWLClassQ.getClassIri(), et.getIRI().toString());
    }

    private MetamodelImpl metamodelFor(Class<?>... classes) {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(new HashSet<>(Arrays.asList(classes)));
        final MetamodelImpl metamodel = new MetamodelImpl(conf);
        metamodel.build(entityLoaderMock);
        return metamodel;
    }

    @Test
    public void entityWithMappedSuperclassIsBuiltWithAllRelevantFields() throws Exception {
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
    public void entityWithMappedSuperclassSetsEntityTypeSupertype() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassQ.class);
        final EntityType<OWLClassQ> et = metamodel.entity(OWLClassQ.class);

        final IdentifiableType<? super OWLClassQ> supertype = et.getSupertype();
        assertEquals(QMappedSuperclass.class, supertype.getJavaType());
    }

    @Test
    public void entityWithAbstractEntityParentSetsEntityTypeSupertype() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, OWLClassS.class);

        final EntityType<OWLClassR> etR = metamodel.entity(OWLClassR.class);
        assertNotNull(etR);
        final EntityType<OWLClassS> etS = metamodel.entity(OWLClassS.class);
        assertNotNull(etS);
        assertEquals(etS, etR.getSupertype());
    }

    @Test
    public void entityWithEntitySuperclassIsBuiltWithAllRelevantAttributes() throws Exception {
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
    public void buildingMetamodelSetsSubtypesOfMappedSuperclass() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassQ.class);
        final IdentifiableType<? super OWLClassQ> supertype = metamodel.entity(OWLClassQ.class).getSupertype();
        assertTrue(supertype instanceof AbstractIdentifiableType);
        final AbstractIdentifiableType<? super OWLClassQ> mappedSupertype = (AbstractIdentifiableType<? super OWLClassQ>) supertype;
        assertTrue(mappedSupertype.hasSubtypes());
        assertTrue(mappedSupertype.getSubtypes().contains(metamodel.entity(OWLClassQ.class)));
    }

    @Test
    public void buildingMetamodelSetsSubtypesOfEntitySuperclass() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, OWLClassS.class);
        final EntityTypeImpl<OWLClassS> supertype = metamodel.entity(OWLClassS.class);
        assertTrue(supertype.hasSubtypes());
        assertTrue(supertype.getSubtypes().contains(metamodel.entity(OWLClassR.class)));
    }

    @Test
    public void subtypesOfEntityWithoutSubtypesAreEmpty() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassA.class);
        final EntityTypeImpl<OWLClassA> et = metamodel.entity(OWLClassA.class);
        assertFalse(et.hasSubtypes());
        assertTrue(et.getSubtypes().isEmpty());
    }

    @Test
    public void buildingMetamodelResolvesInheritanceStrategy() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, OWLClassS.class);
        final Inheritance inheritance = OWLClassS.class.getDeclaredAnnotation(Inheritance.class);
        final EntityTypeImpl<OWLClassS> sEntityType = metamodel.entity(OWLClassS.class);
        assertEquals(inheritance.strategy(), sEntityType.getInheritanceType());
        final EntityTypeImpl<OWLClassR> rEntityType = metamodel.entity(OWLClassR.class);
        assertEquals(inheritance.strategy(), rEntityType.getInheritanceType());
    }

    @Test
    public void buildingMetamodelSetsDefaultInheritanceStrategyWhenItIsNotSpecifiedOnEntity() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassA.class);
        final EntityTypeImpl<OWLClassA> et = metamodel.entity(OWLClassA.class);
        assertEquals(Constants.DEFAULT_INHERITANCE_TYPE, et.getInheritanceType());
    }

    @Test
    public void buildingMetamodelThrowsExceptionWhenInheritanceStrategyIsDeclaredOnSubtype() throws Exception {
        thrown.expect(MetamodelInitializationException.class);
        thrown.expectMessage("Class " + SubclassWithInheritanceType.class +
                " cannot declare inheritance strategy, because it already inherits it from its supertype.");
        metamodelFor(OWLClassS.class, SubclassWithInheritanceType.class);
    }

    @Inheritance(strategy = InheritanceType.TRY_FIRST)
    @OWLClass(iri = "http://krizik.felk.cvut.cz/ontologies/jopa/entities#SubclassWithInheritanceType")
    private static final class SubclassWithInheritanceType extends OWLClassS {

    }

    @Test
    public void buildingMetamodelSetsMultipleSubtypesOnSuperType() throws Exception {
        final MetamodelImpl metamodel = metamodelFor(OWLClassR.class, AnotherSubclass.class, OWLClassS.class);
        final EntityTypeImpl<OWLClassS> supertype = metamodel.entity(OWLClassS.class);
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
    public void buildingMetamodelSupportsReferenceFromParentEntityToSubEntity() {
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
