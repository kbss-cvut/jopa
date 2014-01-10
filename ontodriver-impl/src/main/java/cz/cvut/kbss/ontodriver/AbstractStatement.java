package cz.cvut.kbss.ontodriver;

public abstract class AbstractStatement implements DriverStatement {

	protected final JopaStatement jopaStatement;
	protected final String query;
	protected final boolean useTransactionalOntology;

	public AbstractStatement(JopaStatement statement) {
		if (statement == null) {
			throw new NullPointerException();
		}
		this.jopaStatement = statement;
		this.query = statement.getQuery();
		this.useTransactionalOntology = statement.useTransactionalOntology();
	}

	public boolean shouldUseTransactionalOntology() {
		return useTransactionalOntology;
	}
}
