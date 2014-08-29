package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.PreparedStatement;

public class SesamePreparedStatement extends SesameStatement implements PreparedStatement {

	// TODO We're not doing any escaping

	private String[] paramValues;
	private List<String> statementParts;
	private final String statement;

	public SesamePreparedStatement(StatementExecutor executor, String statement)
			throws OntoDriverException {
		super(executor);
		this.statement = statement.trim();
		if (statement.isEmpty()) {
			throw new IllegalArgumentException("The statement string cannot be empty.");
		}
		analyzeStatement();
	}

	private void analyzeStatement() throws OntoDriverException {
		this.statementParts = new ArrayList<>();
		// In single-quoted string
		boolean inSQString = false;
		// In double-quoted string
		boolean inDQString = false;
		int lastParamEndIndex = 0;
		for (int i = 0; i < statement.length(); i++) {
			final char c = statement.charAt(i);
			switch (c) {
			case '\'':
				inSQString = !inSQString;
				break;
			case '"':
				inDQString = !inDQString;
				break;
			case '?':
				if (!inSQString && !inDQString) {
					statementParts.add(statement.substring(lastParamEndIndex, i));
					lastParamEndIndex = i + 1;
				}
			default:
				break;
			}
		}
		statementParts.add(statement.substring(lastParamEndIndex));
		this.paramValues = new String[statementParts.size() - 1];
		clearParameters();

	}

	@Override
	public void setObject(int parameterIndex, Object value) throws OntoDriverException {
		ensureOpen();
		if (parameterIndex < 0 || parameterIndex > paramValues.length) {
			throw new IndexOutOfBoundsException("Parameter index value " + parameterIndex
					+ " out of bounds.");
		}
		paramValues[parameterIndex] = value.toString();
	}

	@Override
	public cz.cvut.kbss.ontodriver.ResultSet executeQuery() throws OntoDriverException {
		ensureOpen();
		return executeQuery(assembleStatement());
	}

	private String assembleStatement() throws SesameDriverException {
		final StringBuilder sb = new StringBuilder(statement.length());
		for (int i = 0; i < paramValues.length; i++) {
			sb.append(statementParts.get(i));
			if (paramValues[i] == null) {
				throw new SesameDriverException("Missing value of parameter at index " + i);
			}
			sb.append(paramValues[i]);
		}
		sb.append(statementParts.get(paramValues.length));
		return sb.toString();
	}

	@Override
	public void executeUpdate() throws OntoDriverException {
		ensureOpen();
		executeUpdate(assembleStatement());
	}

	@Override
	public void clearParameters() throws OntoDriverException {
		Arrays.fill(paramValues, null);
	}
}
