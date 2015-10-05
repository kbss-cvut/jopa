package cz.cvut.kbss.jopa.test.query.runner;

import cz.cvut.kbss.jopa.model.EntityManager;

import java.util.logging.Logger;

public abstract class BaseQueryRunner {

    protected final Logger logger;

    protected BaseQueryRunner(Logger logger) {
        assert logger != null;
        this.logger = logger;
    }

    protected abstract EntityManager getEntityManager();
}
