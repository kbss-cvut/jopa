/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.QMappedSuperclass;
import cz.cvut.kbss.jopa.loaders.EntityLoader;
import cz.cvut.kbss.jopa.model.metamodel.Attribute;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableType;
import cz.cvut.kbss.jopa.utils.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.lang.reflect.Field;
import java.util.*;

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
        final MetamodelImpl metamodel = metamodelFor(OWLClassQ.class);
        final EntityType<OWLClassQ> et = metamodel.entity(OWLClassQ.class);
        assertNotNull(et);
        assertEquals(OWLClassQ.getClassIri(), et.getIRI().toString());
    }

    private MetamodelImpl metamodelFor(Class<?>... classes) {
        when(entityLoaderMock.discoverEntityClasses(conf)).thenReturn(new HashSet<>(Arrays.asList(classes)));
        return new MetamodelImpl(conf, entityLoaderMock);
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
}
