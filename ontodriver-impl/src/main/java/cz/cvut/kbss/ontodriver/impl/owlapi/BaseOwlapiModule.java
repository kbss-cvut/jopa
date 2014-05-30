package cz.cvut.kbss.ontodriver.impl.owlapi;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import cz.cvut.kbss.jopa.model.EntityDescriptor;
import cz.cvut.kbss.jopa.utils.ErrorUtils;
import cz.cvut.kbss.ontodriver.DriverFactory;
import cz.cvut.kbss.ontodriver.JopaStatement;
import cz.cvut.kbss.ontodriver.PersistenceProviderFacade;
import cz.cvut.kbss.ontodriver.ResultSet;
import cz.cvut.kbss.ontodriver.StorageModule;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.ModuleInternal;

public abstract class BaseOwlapiModule extends StorageModule implements OwlapiModuleWrapper {

	protected ModuleInternal<OWLOntologyChange, OwlapiStatement> internal;

	public BaseOwlapiModule(PersistenceProviderFacade persistenceProvider, DriverFactory factory)
			throws OntoDriverException {
		super(persistenceProvider, factory);
	}

	@Override
	public void rollback() throws OntoDriverException {
		ensureOpen();
		internal.rollback();
		this.transaction = TransactionState.NO;
	}

	@Override
	public boolean contains(Object primaryKey, URI context) throws OntoDriverException {
		preContains(primaryKey, context);
		return internal.containsEntity(primaryKey, context);
	}

	@Override
	public <T> T find(Class<T> cls, Object primaryKey, EntityDescriptor descriptor)
			throws OntoDriverException {
		preFind(cls, primaryKey, descriptor);

		return internal.findEntity(cls, primaryKey, descriptor);
	}

	@Override
	public List<URI> getContexts() {
		ensureOpen();
		return Collections.emptyList();
	}

	@Override
	public boolean isConsistent(URI context) throws OntoDriverException {
		preIsConsistent(context);

		return internal.isConsistent(context);
	}

	@Override
	public <T> void loadFieldValue(T entity, Field field, EntityDescriptor descriptor)
			throws OntoDriverException {
		preLoadFieldValue(entity, field, descriptor);
		internal.loadFieldValue(entity, field, descriptor);
	}

	@Override
	public <T> void merge(T entity, Field mergedField, EntityDescriptor descriptor)
			throws OntoDriverException {
		preMerge(entity, mergedField, descriptor);

		internal.mergeEntity(entity, mergedField, descriptor);
	}

	@Override
	public <T> void persist(Object primaryKey, T entity, EntityDescriptor descriptor)
			throws OntoDriverException {
		prePersist(entity, descriptor);
		internal.persistEntity(primaryKey, entity, descriptor);
	}

	@Override
	public void remove(Object primaryKey, EntityDescriptor descriptor) throws OntoDriverException {
		preRemove(primaryKey, descriptor);

		internal.removeEntity(primaryKey, descriptor);
	}

	@Override
	public ResultSet executeStatement(JopaStatement statement) throws OntoDriverException {
		ensureOpen();
		startTransactionIfNotActive();
		final OwlapiStatement stmt = (OwlapiStatement) factory.createStatement(statement);
		return internal.executeStatement(stmt);
	}

	@Override
	public <T> T getEntityFromCache(Class<T> cls, Object primaryKey) {
		Objects.requireNonNull(cls, ErrorUtils.constructNPXMessage("cls"));
		Objects.requireNonNull(primaryKey, ErrorUtils.constructNPXMessage("primaryKey"));
		// OWLAPI repositories don't use contexts
		final EntityDescriptor descriptor = new EntityDescriptor();

		return getEntityFromProviderCache(cls, primaryKey, descriptor);
	}

}