package cz.cvut.kbss.jopa;

import cz.cvut.kbss.jopa.model.*;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;

public class PersistenceTest {

    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void createEmfWithPropertiesInstantiatesPersistenceProviderFromProperties() {
        DefaultPersistenceProviderResolver.registerPersistenceProviderClass(TestPersistenceProvider.class);
        final Map<String, String> props = Collections.singletonMap(PersistenceProperties.JPA_PERSISTENCE_PROVIDER,
                TestPersistenceProvider.class.getName());
        Persistence.createEntityManagerFactory("testPU", props);
        assertNotNull(TestPersistenceProvider.instance);
        assertEquals(1, TestPersistenceProvider.instance.createEmfCalled);
    }

    @Test
    public void createEmfThrowsIllegalArgumentWhenProviderIsNotConfigured() {
        DefaultPersistenceProviderResolver.registerPersistenceProviderClass(TestPersistenceProvider.class);
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage("Missing persistence unit provider.");
        Persistence.createEntityManagerFactory("testPU", Collections.emptyMap());
    }

    @Test
    public void createEmfThrowsIllegalArgumentWhenConfiguredClassIsNotPersistenceProvider() {
        thrown.expect(IllegalArgumentException.class);
        thrown.expectMessage(
                "Type " + PersistenceTest.class.getName() + " is not a PersistenceProvider implementation.");
        final Map<String, String> props = Collections.singletonMap(PersistenceProperties.JPA_PERSISTENCE_PROVIDER,
                PersistenceTest.class.getName());
        Persistence.createEntityManagerFactory("testPU", props);
    }

    public static class TestPersistenceProvider implements PersistenceProvider {

        private static TestPersistenceProvider instance;

        private EntityManagerFactory emfMock = mock(EntityManagerFactory.class);
        private ProviderUtil providerUtilMock = mock(ProviderUtil.class);

        private int createEmfCalled = 0;
        private int getProviderUtilCalled = 0;

        public TestPersistenceProvider() {
            instance = this;
        }

        @Override
        public EntityManagerFactory createEntityManagerFactory(String emName, Map<String, String> map) {
            createEmfCalled++;
            return emfMock;
        }

        @Override
        public ProviderUtil getProviderUtil() {
            getProviderUtilCalled++;
            return providerUtilMock;
        }
    }
}