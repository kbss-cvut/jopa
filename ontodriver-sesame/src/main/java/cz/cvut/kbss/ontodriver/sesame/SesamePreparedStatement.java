package cz.cvut.kbss.ontodriver.sesame;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.sesame.connector.StatementExecutor;
import cz.cvut.kbss.ontodriver.sesame.exceptions.SesameDriverException;
import cz.cvut.kbss.ontodriver_new.PreparedStatement;

public class SesamePreparedStatement extends SesameStatement implements PreparedStatement {

	// TODO We're not doing any escaping
	// TODO Extract a common superclass into api, since this kind of code will
	// repeat in all drivers

	private Map<String, String> paramValues;
	private List<String> paramNames;
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
		this.paramNames = new ArrayList<>();
		this.statementParts = new ArrayList<>();
		// In single-quoted string
		boolean inSQString = false;
		// In double-quoted string
		boolean inDQString = false;
		boolean inParam = false;
		int lastParamEndIndex = 0;
		int paramStartIndex = 0;
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
					paramStartIndex = i + 1;
					inParam = true;
				}
				break;
			case '\n':
			case ' ':
				if (inParam) {
					lastParamEndIndex = i;
					inParam = false;
					final String param = statement.substring(paramStartIndex, i);
					paramNames.add(param);
				}
				break;
			default:
				break;
			}
		}
		statementParts.add(statement.substring(lastParamEndIndex));
		this.paramValues = new HashMap<>(paramNames.size());

		assert statementParts.size() == paramNames.size() + 1;

	}

	@Override
	public void setObject(String binding, Object value) throws OntoDriverException {
		ensureOpen();
		if (!paramNames.contains(binding)) {
			throw new IllegalArgumentException("Unknown binding name " + binding);
		}
		paramValues.put(binding, value.toString());
	}

	@Override
	public cz.cvut.kbss.ontodriver.ResultSet executeQuery() throws OntoDriverException {
		ensureOpen();
		return executeQuery(assembleStatement());
	}

	private String assembleStatement() throws SesameDriverException {
		final StringBuilder sb = new StringBuilder(statement.length());
		for (int i = 0; i < paramNames.size(); i++) {
			sb.append(statementParts.get(i));
			final String paramValue = paramValues.get(paramNames.get(i));
			if (paramValue == null) {
				sb.append("?").append(paramNames.get(i));
			} else {
				sb.append(paramValue);
			}
		}
		sb.append(statementParts.get(paramNames.size()));
		return sb.toString();
	}

	@Override
	public void executeUpdate() throws OntoDriverException {
		ensureOpen();
		executeUpdate(assembleStatement());
	}

	@Override
	public void clearParameters() throws OntoDriverException {
		for (String param : paramValues.keySet()) {
			paramValues.put(param, null);
		}
	}
}
