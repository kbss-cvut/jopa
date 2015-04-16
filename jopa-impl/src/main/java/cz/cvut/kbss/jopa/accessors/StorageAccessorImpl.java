package cz.cvut.kbss.jopa.accessors;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.ServerSession;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.DataSource;
import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.Map;

public class StorageAccessorImpl implements StorageAccessor {

    private static final String DATA_SOURCE_CLASS = "cz.cvut.kbss.ontodriver.impl.SimpleDataSource";

    private final Metamodel metamodel;
    private final ServerSession serverSession;
    private final PersistenceProviderFacade provider;

    private DataSource dataSource;
    private boolean open;

    private StorageAccessorImpl(Metamodel metamodel, ServerSession serverSession) {
        super();
        if (metamodel == null || serverSession == null) {
            throw new NullPointerException();
        }
        this.metamodel = metamodel;
        this.serverSession = serverSession;
        this.provider = initPersistenceProvider();
        this.open = true;
    }

    public StorageAccessorImpl(Metamodel metamodel, ServerSession serverSession,
                               OntologyStorageProperties storageProperties) {
        this(metamodel, serverSession);
        initDataSource(storageProperties, Collections.<String, String>emptyMap());
    }

    public StorageAccessorImpl(Metamodel metamodel, ServerSession serverSession,
                               OntologyStorageProperties storageProps, Map<String, String> properties) {
        this(metamodel, serverSession);
        initDataSource(storageProps, properties);
    }

    @Override
    public synchronized Connection acquireConnection() {
        try {
            final Connection conn = dataSource.getConnection(provider);
            conn.setAutoCommit(false);
            return conn;
        } catch (OntoDriverException e) {
            throw new OWLPersistenceException(e);
        }
    }

    private void initDataSource(OntologyStorageProperties storageProps,
                                Map<String, String> properties) {
        try {
            final Class<?> dsClass = Class.forName(DATA_SOURCE_CLASS);
            final Constructor<?> constructor = dsClass.getConstructor(OntologyStorageProperties.class, Map.class);
            this.dataSource = (DataSource) constructor.newInstance(storageProps, properties);
        } catch (ClassNotFoundException | NoSuchMethodException | InvocationTargetException
                | InstantiationException | IllegalAccessException e) {
            throw new OWLPersistenceException("Unable to load OntoDriver data source class.", e);
        }
    }

    private PersistenceProviderFacade initPersistenceProvider() {
        return new PersistenceProviderProxy(metamodel, serverSession);
    }

    @Override
    public void close() throws OntoDriverException {
        if (!open) {
            return;
        }
        if (dataSource != null) {
            dataSource.close();
        }
        this.open = false;
    }

    @Override
    public boolean isOpen() {
        return open;
    }
}
