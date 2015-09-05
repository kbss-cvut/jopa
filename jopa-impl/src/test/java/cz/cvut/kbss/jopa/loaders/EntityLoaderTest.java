package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import org.junit.Test;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * @author ledvima1
 */
public class EntityLoaderTest {

    private static final Set<Class<?>> ENTITY_CLASSES = initEntityClasses();

    private static Set<Class<?>> initEntityClasses() {
        final Set<Class<?>> set = new HashSet<>();
        set.add(OWLClassA.class);
        set.add(OWLClassB.class);
        set.add(OWLClassC.class);
        set.add(OWLClassD.class);
        set.add(OWLClassE.class);
        set.add(OWLClassF.class);
        set.add(OWLClassG.class);
        set.add(OWLClassH.class);
        set.add(OWLClassI.class);
        set.add(OWLClassJ.class);
        set.add(OWLClassK.class);
        set.add(OWLClassL.class);
        set.add(OWLClassM.class);
        return set;
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenScanPackageIsNotSupplied() throws Exception {
        final Map<String, String> properties = Collections.emptyMap();
        EntityLoader.discoverEntityClasses(properties);
    }

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenScanPackageIsEmpty() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                OWLAPIPersistenceProperties.SCAN_PACKAGE, "");
        EntityLoader.discoverEntityClasses(properties);
    }

    @Test
    public void doesNotFailWhenUnknownPackageNameIsPassed() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                OWLAPIPersistenceProperties.SCAN_PACKAGE, "com.cvut");
        final Set<Class<?>> result = EntityLoader.discoverEntityClasses(properties);
        assertTrue(result.isEmpty());
    }

    @Test
    public void loadsEntityClassesWhenCorrectPackageIsSet() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                OWLAPIPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss.jopa.environment");
        final Set<Class<?>> result = EntityLoader.discoverEntityClasses(properties);
        assertEquals(ENTITY_CLASSES, result);
    }

    @Test
    public void loadsEntityClassesWhenAncestorPackageIsSet() throws Exception {
        final Map<String, String> properties = Collections.singletonMap(
                OWLAPIPersistenceProperties.SCAN_PACKAGE, "cz.cvut.kbss");
        final Set<Class<?>> result = EntityLoader.discoverEntityClasses(properties);
        assertEquals(ENTITY_CLASSES, result);
    }
}