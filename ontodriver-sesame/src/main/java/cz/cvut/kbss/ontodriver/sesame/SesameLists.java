package cz.cvut.kbss.ontodriver.sesame;

import static cz.cvut.kbss.jopa.utils.ErrorUtils.constructNPXMessage;

import java.util.List;
import java.util.Objects;

import cz.cvut.kbss.ontodriver.exception.OntoDriverException;
import cz.cvut.kbss.ontodriver.Lists;
import cz.cvut.kbss.ontodriver.descriptor.ListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.ReferencedListValueDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListDescriptor;
import cz.cvut.kbss.ontodriver.descriptor.SimpleListValueDescriptor;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

class SesameLists implements Lists {

	private final SesameConnection connection;
	private final SesameAdapter adapter;

	public SesameLists(SesameConnection connection, SesameAdapter adapter) {
		this.connection = connection;
		this.adapter = adapter;
	}

	@Override
	public List<Axiom<NamedResource>> loadSimpleList(SimpleListDescriptor descriptor)
			throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		return adapter.getSimpleListHandler().loadList(descriptor);
	}

	private void verifyArgs(ListDescriptor descriptor, String argName) {
		connection.ensureOpen();
		Objects.requireNonNull(descriptor, constructNPXMessage(argName));
	}

	@Override
	public void persistSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		adapter.getSimpleListHandler().persistList(descriptor);
		connection.commitIfAuto();
	}

	@Override
	public void updateSimpleList(SimpleListValueDescriptor descriptor) throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		adapter.getSimpleListHandler().updateList(descriptor);
		connection.commitIfAuto();
	}

	@Override
	public List<Axiom<NamedResource>> loadReferencedList(ReferencedListDescriptor descriptor)
			throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		return adapter.getReferencedListHandler().loadList(descriptor);
	}

	@Override
	public void persistReferencedList(ReferencedListValueDescriptor descriptor)
			throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		adapter.getReferencedListHandler().persistList(descriptor);
		connection.commitIfAuto();
	}

	@Override
	public void updateReferencedList(ReferencedListValueDescriptor descriptor)
			throws OntoDriverException {
		verifyArgs(descriptor, "descriptor");
		adapter.getReferencedListHandler().updateList(descriptor);
		connection.commitIfAuto();
	}
}
