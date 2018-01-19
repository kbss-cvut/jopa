package cz.cvut.kbss.ontodriver.jena.connector;

import cz.cvut.kbss.ontodriver.config.Configuration;
import cz.cvut.kbss.ontodriver.exception.OntoDriverInitializationException;
import cz.cvut.kbss.ontodriver.jena.exception.JenaDriverException;
import org.apache.jena.query.DatasetFactory;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.riot.RDFDataMgr;
import org.apache.jena.riot.RDFLanguages;
import org.apache.jena.riot.RiotNotFoundException;
import org.apache.jena.util.FileUtils;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * File storage accessor.
 * <p>
 * Note that currently this accessor does not support working with datasets. Only single graph can be present in the file.
 */
class FileStorage extends Storage {

    private final String location;

    FileStorage(Configuration configuration) {
        this.location = configuration.getStorageProperties().getPhysicalURI().toString();
    }

    @Override
    public void initialize() {
        try {
            try {
                initDataset();
            } catch (RiotNotFoundException e) {
                tryCreatingFile();
            }
        } catch (RuntimeException e) {
            throw new OntoDriverInitializationException("Unable to initialize file storage at " + location, e);
        }
    }

    private void initDataset() {
        final Model model = RDFDataMgr.loadModel(location);
        this.dataset = DatasetFactory.create(model);
    }

    private void tryCreatingFile() {
        final File file = new File(location);
        try {
            final boolean result = file.createNewFile();
            assert result;
        } catch (IOException e) {
            LOG.error("Unable to create storage file {}.", location);
            throw new OntoDriverInitializationException("Unable to initialize file storage at " + location, e);
        }
        initDataset();
    }

    @Override
    void writeChanges() throws JenaDriverException {
        try (final BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(location))) {
            final String language = FileUtils.guessLang(location);
            RDFDataMgr.write(out, dataset.getDefaultModel(), RDFLanguages.nameToLang(language));
        } catch (IOException e) {
            throw new JenaDriverException("Unable to write out dataset changes.", e);
        }
    }
}
