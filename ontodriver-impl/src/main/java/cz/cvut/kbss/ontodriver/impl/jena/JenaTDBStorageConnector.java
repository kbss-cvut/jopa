package cz.cvut.kbss.ontodriver.impl.jena;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import org.semanticweb.owlapi.model.OWLOntologyChange;

import com.hp.hpl.jena.query.Dataset;
import com.hp.hpl.jena.query.ReadWrite;
import com.hp.hpl.jena.tdb.TDB;
import com.hp.hpl.jena.tdb.TDBFactory;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;
import cz.cvut.kbss.ontodriver.impl.owlapi.OwlapiConnectorDataHolder;

public class JenaTDBStorageConnector extends JenaStorageConnector {

	private Dataset dataset;

	public JenaTDBStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(storageProperties, properties);
	}

	@Override
	protected void initConnector() throws OntoDriverException {
		final File f = new File(physicalUri);
		this.dataset = TDBFactory.createDataset(f.getAbsolutePath());
		// Maybe the named model should be retrieved?
		dataset.begin(ReadWrite.WRITE);
		this.model = dataset.getDefaultModel();
		dataset.commit();
	}

	@Override
	public OwlapiConnectorDataHolder getOntologyDataInOwlapi()
			throws OntoDriverException {
		return super.getOntologyDataInOwlapi();
	}

	@Override
	public void applyOntologyChanges(List<OWLOntologyChange> changes)
			throws OntoDriverException {
		dataset.begin(ReadWrite.WRITE);
		model.begin();
		super.applyOntologyChanges(changes);
	}

	@Override
	public void close() throws OntoDriverException {
		if (!isOpen()) {
			return;
		}
		super.close();
		if (dataset.isInTransaction()) {
			dataset.abort();
		}
		dataset.close();
	}

	@Override
	public void saveOntology() throws OntoDriverException {
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Committing changes to dataset in directory "
					+ physicalUri);
		}
		model.commit();
		dataset.commit();
	}

	@Override
	public void reload() throws OntoDriverException {
		if (dataset.isInTransaction()) {
			dataset.abort();
		}
		TDB.sync(dataset);
		// Force reloading OWL API ontology
		getOntologyDataInOwlapi();
	}

}
