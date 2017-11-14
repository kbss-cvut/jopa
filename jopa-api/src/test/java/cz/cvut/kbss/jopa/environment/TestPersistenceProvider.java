package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.model.EntityManagerFactory;
import cz.cvut.kbss.jopa.model.PersistenceProvider;
import cz.cvut.kbss.jopa.model.ProviderUtil;

import java.util.Map;

public class TestPersistenceProvider implements PersistenceProvider {

    @Override
    public EntityManagerFactory createEntityManagerFactory(String emName, Map<String, String> map) {
        return null;
    }

    @Override
    public ProviderUtil getProviderUtil() {
        return null;
    }
}
