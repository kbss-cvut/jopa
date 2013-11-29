package cz.cvut.kbss.ontodriver.impl.sesame;

import java.lang.reflect.Field;
import java.util.List;

import org.openrdf.model.Model;

import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

class SesameModuleInternal implements ModuleInternal<SesameChange, SesameStatement> {

	SesameModuleInternal(Model model) {
		// TODO Auto-generated constructor stub
	}

	@Override
	public boolean containsEntity(Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> T findEntity(Class<T> cls, Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public boolean isConsistent() throws OntoDriverException {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public <T> void persistEntity(Object primaryKey, T entity) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void mergeEntity(Object primaryKey, T entity) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void removeEntity(Object primaryKey) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public <T> void loadFieldValue(T entity, Field field) throws OntoDriverException {
		// TODO Auto-generated method stub

	}

	@Override
	public void rollback() {
		// TODO Auto-generated method stub

	}

	@Override
	public void reset() {
		// TODO Auto-generated method stub

	}

	@Override
	public ResultSet executeStatement(SesameStatement statement) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public List<SesameChange> commitAndRetrieveChanges() {
		// TODO Auto-generated method stub
		return null;
	}

}
