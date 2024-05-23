package cz.cvut.kbss.ontodriver.rdf4j.connector;

import org.eclipse.rdf4j.common.transaction.IsolationLevel;

/**
 * Configuration for the {@link ConnectionFactoryImpl}.
 * @param isGraphDB Whether the underlying repository is GraphDB
 * @param txIsolationLevel Configured transaction isolation level, possibly {@code null}
 */
public record ConnectionFactoryConfig(boolean isGraphDB, IsolationLevel txIsolationLevel) {
}
