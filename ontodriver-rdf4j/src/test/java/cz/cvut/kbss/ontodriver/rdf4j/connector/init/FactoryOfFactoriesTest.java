package cz.cvut.kbss.ontodriver.rdf4j.connector.init;

import cz.cvut.kbss.ontodriver.config.DriverConfiguration;
import cz.cvut.kbss.ontodriver.rdf4j.config.Rdf4jConfigParam;
import cz.cvut.kbss.ontodriver.rdf4j.connector.ConnectionFactoryConfig;
import cz.cvut.kbss.ontodriver.rdf4j.environment.TestUtils;
import org.eclipse.rdf4j.common.transaction.IsolationLevels;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class FactoryOfFactoriesTest {

    @Test
    void initializationResolvesThatUnderlyingRepoIsNotGraphDBWhenItDoesNotContainGraphDBInternalIdStatements() throws Exception {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        final FactoryOfFactories sut = new FactoryOfFactories(conf);
        assertFalse(sut.resolveFactoryConfig().isGraphDB());
    }

    @Test
    void resolveFactoryConfigResolvesTransactionIsolationLevelFromConfiguration() throws Exception {
        final DriverConfiguration conf = TestUtils.createDriverConfig("test");
        conf.setProperty(Rdf4jConfigParam.USE_VOLATILE_STORAGE, Boolean.TRUE.toString());
        conf.setProperty(Rdf4jConfigParam.TRANSACTION_ISOLATION_LEVEL, IsolationLevels.SERIALIZABLE.toString());
        final FactoryOfFactories sut = new FactoryOfFactories(conf);
        final ConnectionFactoryConfig result = sut.resolveFactoryConfig();
        assertEquals(IsolationLevels.SERIALIZABLE, result.txIsolationLevel());
    }
}
