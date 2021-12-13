package cz.cvut.kbss.jopa.datatype.xsd;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

/**
 * Provides a shared instance of {@link javax.xml.datatype.DatatypeFactory}, so that individual mappers do not need to manage their own.
 * <p>
 * Note that this assumes the factory implementation is thread-safe.
 */
class DatatypeFactoryProvider {

    private static final DatatypeFactory FACTORY = initFactory();

    private static DatatypeFactory initFactory() {
        try {
            return DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException e) {
            throw new OWLPersistenceException("Unable to initialize javax.xml.datatype.DatatypeFactory.", e);
        }
    }

    static DatatypeFactory getFactory() {
        return FACTORY;
    }
}
