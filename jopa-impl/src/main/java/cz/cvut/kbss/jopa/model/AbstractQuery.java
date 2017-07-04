package cz.cvut.kbss.jopa.model;

import cz.cvut.kbss.jopa.exceptions.OWLPersistenceException;
import cz.cvut.kbss.jopa.query.QueryHolder;
import cz.cvut.kbss.jopa.sessions.ConnectionWrapper;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.jopa.utils.Procedure;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * Common state and behavior of both {@link cz.cvut.kbss.jopa.model.query.Query} and {@link
 * cz.cvut.kbss.jopa.model.query.TypedQuery} implementations.
 */
abstract class AbstractQuery {

    static final Logger LOG = LoggerFactory.getLogger(AbstractQuery.class);

    final QueryHolder query;
    final ConnectionWrapper connection;

    private boolean useBackupOntology;

    private Procedure rollbackOnlyMarker;

    AbstractQuery(QueryHolder query, ConnectionWrapper connection) {
        this.query = Objects.requireNonNull(query, ErrorUtils.getNPXMessageSupplier("query"));
        this.connection = Objects.requireNonNull(connection, ErrorUtils.getNPXMessageSupplier("connection"));
        this.useBackupOntology = false;
    }

    /**
     * Sets ontology used for processing of this query.
     *
     * @param useBackupOntology If true, the backup (central) ontology is used, otherwise the transactional ontology is
     *                          used (default)
     */
    public void useBackupOntology(boolean useBackupOntology) {
        this.useBackupOntology = useBackupOntology;
    }

    void executeUpdateImpl() {
        final Statement stmt = connection.createStatement();
        try {
            setTargetOntology(stmt);
            stmt.executeUpdate(query.assembleQuery());
        } catch (OntoDriverException e) {
            markTransactionForRollback();
            throw queryEvaluationException(e);
        } catch (RuntimeException e) {
            markTransactionForRollback();
            throw e;
        } finally {
            try {
                stmt.close();
            } catch (Exception e) {
                LOG.error("Unable to close statement after update execution.", e);
            }
        }
    }

    void setTargetOntology(Statement stmt) {
        if (useBackupOntology) {
            stmt.useOntology(Statement.StatementOntology.CENTRAL);
        } else {
            stmt.useOntology(Statement.StatementOntology.TRANSACTIONAL);
        }
    }

    OWLPersistenceException queryEvaluationException(OntoDriverException e) {
        final String executedQuery = query.assembleQuery();
        return new OWLPersistenceException("Exception caught when evaluating query " + executedQuery, e);
    }

    void markTransactionForRollback() {
        if (rollbackOnlyMarker != null) {
            rollbackOnlyMarker.execute();
        }
    }

    /**
     * Registers reference to a method which marks current transaction (if active) for rollback on exceptions.
     *
     * @param rollbackOnlyMarker The marker to invoke on exceptions
     */
    void setRollbackOnlyMarker(Procedure rollbackOnlyMarker) {
        this.rollbackOnlyMarker = rollbackOnlyMarker;
    }

    static IllegalStateException unboundParam(Object param) {
        return new IllegalStateException("Parameter " + param + " is not bound.");
    }
}
