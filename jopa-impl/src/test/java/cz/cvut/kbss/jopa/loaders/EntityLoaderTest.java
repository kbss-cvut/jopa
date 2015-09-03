package cz.cvut.kbss.jopa.loaders;

import cz.cvut.kbss.jopa.owlapi.OWLAPIPersistenceProperties;
import org.junit.Test;

import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * @author ledvima1
 */
public class EntityLoaderTest {

    @Test(expected = IllegalArgumentException.class)
    public void throwsExceptionWhenScanPackageIsNotSupplied() throws Exception {
        final Map<String, String> properties = Collections.emptyMap();
        EntityLoader.discoverEntityClassesNew(properties);
    }
}