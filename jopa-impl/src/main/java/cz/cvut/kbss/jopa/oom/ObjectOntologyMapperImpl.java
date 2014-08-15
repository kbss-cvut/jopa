package cz.cvut.kbss.jopa.oom;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.Collection;
import java.util.Objects;

import cz.cvut.kbss.jopa.exceptions.StorageAccessException;
import cz.cvut.kbss.jopa.model.descriptors.Descriptor;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.sessions.UnitOfWorkImpl;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver_new.AxiomDescriptor;
import cz.cvut.kbss.ontodriver_new.Connection;
import cz.cvut.kbss.ontodriver_new.model.Axiom;

class ObjectOntologyMapperImpl implements ObjectOntologyMapper {

	private final UnitOfWorkImpl uow;
	private final Connection storageConnection;
	private final Metamodel metamodel;

	private final AxiomDescriptorFactory descriptorFactory;
	private final EntityConstructor entityBuilder;

	public ObjectOntologyMapperImpl(UnitOfWorkImpl uow, Connection connection) {
		this.uow = Objects.requireNonNull(uow);
		this.storageConnection = Objects.requireNonNull(connection);
		this.metamodel = uow.getMetamodel();
		this.descriptorFactory = new AxiomDescriptorFactory();
		this.entityBuilder = new EntityConstructor(this);
	}

	@Override
	public <T> T loadEntity(Class<T> cls, URI primaryKey, Descriptor descriptor) {
		assert cls != null;
		assert primaryKey != null;
		assert descriptor != null;

		final EntityType<T> et = getEntityType(cls);
		final AxiomDescriptor axiomDescriptor = descriptorFactory.createForEntityLoading(
				primaryKey, descriptor, et);
		try {
			final Collection<Axiom> axioms = storageConnection.find(axiomDescriptor);
			if (axioms.isEmpty()) {
				return null;
			}
			return entityBuilder.reconstructEntity(cls, primaryKey, axioms, et);
		} catch (OntoDriverException e) {
			throw new StorageAccessException(e);
		} catch (InstantiationException | IllegalAccessException e) {
			throw new EntityReconstructionException(e);
		}
	}

	private <T> EntityType<T> getEntityType(Class<T> cls) {
		return metamodel.entity(cls);
	}

	@Override
	public <T> void loadFieldValue(URI primaryKey, T entity, Field field, Descriptor descriptor) {
		assert primaryKey != null;
		assert entity != null;
		assert field != null;
		assert descriptor != null;

		final EntityType<T> et = (EntityType<T>) getEntityType(entity.getClass());
		final AxiomDescriptor axiomDescriptor = descriptorFactory.createForFieldLoading(primaryKey,
				field, descriptor, et);
		try {
			final Collection<Axiom> axioms = storageConnection.find(axiomDescriptor);
			if (axioms.isEmpty()) {
				return;
			}
			entityBuilder.setFieldValue(entity, field, axioms, et);
		} catch (OntoDriverException e) {
			throw new StorageAccessException(e);
		}
	}

}
