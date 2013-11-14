package cz.cvut.kbss.jopa.owlapi.utils;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;
import java.util.Map;

import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.Context;
import cz.cvut.kbss.ontodriver.PreparedStatement;
import cz.cvut.kbss.ontodriver.Statement;
import cz.cvut.kbss.ontodriver.exceptions.EntityNotRegisteredException;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class ConnectionStub implements Connection {

	private Context defaultContext;

	public ConnectionStub() {
		// // Do nothing
	}

	@Override
	public void close() throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public boolean isOpen() {
		// // Do nothing
		return false;
	}

	@Override
	public void commit() throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public void rollback() throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public Statement createStatement() throws OntoDriverException {
		// // Do nothing
		return null;
	}

	@Override
	public boolean contains(Object primaryKey) throws OntoDriverException {
		// // Do nothing
		return false;
	}

	@Override
	public boolean contains(Object primaryKey, URI context) throws OntoDriverException {
		// // Do nothing
		return false;
	}

	@Override
	public boolean isConsistent(URI context) throws OntoDriverException {
		// // Do nothing
		return false;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey) throws OntoDriverException {
		// // Do nothing
		return null;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, URI context) throws OntoDriverException {
		// // Do nothing
		return null;
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, URI entityContext,
			Map<String, URI> attributeContexts) throws OntoDriverException {
		// // Do nothing
		return null;
	}

	@Override
	public boolean getAutoCommit() throws OntoDriverException {
		// // Do nothing
		return false;
	}

	@Override
	public Context getContext(URI contextUri) throws OntoDriverException {
		return defaultContext;
	}

	@Override
	public Context getCurrentContext() throws OntoDriverException {
		return defaultContext;
	}

	@Override
	public List<Context> getContexts() throws OntoDriverException {
		// // Do nothing
		return null;
	}

	@Override
	public Context getSaveContextFor(Object entity) throws OntoDriverException {
		return defaultContext;
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException,
			EntityNotRegisteredException {
		// // Do nothing

	}

	@Override
	public <T> void merge(Object primaryKey, T entity) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public <T> void persist(Object primaryKey, T entity) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public <T> void persist(Object primaryKey, T entity, URI context) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public <T> void persist(Object primaryKey, T entity, URI entityContext,
			Map<String, URI> attributeContexts) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public PreparedStatement prepareStatement(String sparql) throws OntoDriverException {
		// // Do nothing
		return null;
	}

	@Override
	public <T> void registerWithContext(T entity, URI context) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public <T> void remove(Object primaryKey, T entity) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public <T> void remove(Object primaryKey, T entity, URI context) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public void setAutoCommit(boolean autoCommit) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public void setConnectionContext(URI context) throws OntoDriverException {
		// // Do nothing

	}

	@Override
	public void setSaveContextFor(Object entity, URI context) throws OntoDriverException {
		// // Do nothing

	}

	public void setDefaultContext(Context defaultContext) {
		this.defaultContext = defaultContext;
	}

}
