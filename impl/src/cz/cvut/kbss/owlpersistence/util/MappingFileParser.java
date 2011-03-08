package cz.cvut.kbss.owlpersistence.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

public class MappingFileParser {

	public static Map<URI, URI> getMappings(final File mf) {
		final Map<URI, URI> map = new HashMap<URI, URI>();
		String line = null;
		final File defaultDir = mf.getParentFile();
		BufferedReader r;
		try {
			r = new BufferedReader(new InputStreamReader(
					new FileInputStream(mf)));
			while ((line = r.readLine()) != null) {
				final StringTokenizer t = new StringTokenizer(line, ">");
				if (t.countTokens() != 2) {
					System.out
							.println("Ignoring line '" + line
									+ "' - invalid number of tokens="
									+ t.countTokens());
					continue;
				}

				final String uriName = t.nextToken().trim();
				final String fileName = t.nextToken().trim();
				final File actualFile = (new File(fileName).isAbsolute()) ? new File(
						fileName)
						: new File(defaultDir, fileName);

				map.put(URI.create(uriName), actualFile.toURI());
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		return map;
	}
}
