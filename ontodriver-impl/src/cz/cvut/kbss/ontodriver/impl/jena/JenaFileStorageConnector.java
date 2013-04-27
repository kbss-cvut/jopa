package cz.cvut.kbss.ontodriver.impl.jena;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.logging.Level;

import com.hp.hpl.jena.ontology.OntModelSpec;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.util.FileManager;

import cz.cvut.kbss.ontodriver.OntologyStorageProperties;
import cz.cvut.kbss.ontodriver.exceptions.OntoDriverException;

public class JenaFileStorageConnector extends JenaStorageConnector {

	public JenaFileStorageConnector(OntologyStorageProperties storageProperties,
			Map<String, String> properties) throws OntoDriverException {
		super(storageProperties, properties);
	}

	@Override
	protected void initConnector() throws OntoDriverException {
		this.model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);

		final File storageFile = new File(physicalUri);
		if (!storageFile.exists()) {
			try {
				if (LOG.isLoggable(Level.FINE)) {
					LOG.fine("Ontology file at location " + storageFile
							+ " does not exist. Creating empty file.");
				}
				storageFile.createNewFile();
				// No need to try loading model from input stream if the file
				// didn't exist
				return;
			} catch (IOException e) {
				LOG.log(Level.SEVERE, "Unable to create ontology file " + storageFile, e);
				throw new OntoDriverException(e);
			}
		}

		final InputStream in = FileManager.get().open(physicalUri.toString());
		if (in == null) {
			throw new OntoDriverException("Unable to load ontology in location " + physicalUri);
		}
		try {
			if (in.available() == 0) {
				// The file is empty
				return;
			}
		} catch (IOException e) {
			throw new OntoDriverException(e);
		}
		model.read(in, null);
	}

	@Override
	public void reload() throws OntoDriverException {
		initConnector();
		// Force reloading of OWL API ontology
		getOntologyDataInOwlapi();
	}

	@Override
	public void saveOntology() throws OntoDriverException {
		assert model != null;
		if (LOG.isLoggable(Level.FINE)) {
			LOG.fine("Saving ontology " + ontologyUri + " to location " + physicalUri);
		}
		try {
			// We can assume that the file already exists, since it should have
			// been at least created by the initConnector method
			final OutputStream out = new FileOutputStream(new File(physicalUri));
			model.write(out);
		} catch (FileNotFoundException e) {
			throw new OntoDriverException(e);
		}
	}
}
