package com.tle.core.favourites.converter;

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.thoughtworks.xstream.XStream;
import com.tle.beans.Institution;
import com.tle.beans.item.Bookmark;
import com.tle.beans.item.Item;
import com.tle.core.favourites.dao.BookmarkDao;
import com.tle.core.filesystem.BucketFile;
import com.tle.core.filesystem.SubTemporaryFile;
import com.tle.core.filesystem.TemporaryFileHandle;
import com.tle.core.guice.Bind;
import com.tle.core.institution.convert.AbstractMigratableConverter;
import com.tle.core.institution.convert.ConverterParams;
import com.tle.core.services.InstitutionImportService.ConvertType;
import com.tle.core.services.entity.IdOnlyConverter;
import com.tle.core.services.impl.InstitutionImportServiceImpl.ConverterTasks;

@Bind
@Singleton
@SuppressWarnings("nls")
public class FavouriteItemsConverter extends AbstractMigratableConverter<Object>
{
	private static final String MY_FAVOURITES_IMPORT_EXPORT_FOLDER = "myfavourites";

	@Inject
	private BookmarkDao bookmarkDao;

	private XStream xstream;

	@Override
	public void doDelete(Institution institution, ConverterParams callback)
	{
		bookmarkDao.deleteAll();
		bookmarkDao.flush();
		bookmarkDao.clear();
	}

	@Override
	public void addTasks(ConvertType type, ConverterTasks tasks, ConverterParams params)
	{
		if( !params.hasFlag(ConverterParams.NO_ITEMS) )
		{
			super.addTasks(type, tasks, params);
		}
	}

	@Override
	public void doExport(TemporaryFileHandle staging, Institution institution, ConverterParams params)
		throws IOException
	{
		if( !params.hasFlag(ConverterParams.NO_ITEMS) )
		{
			final SubTemporaryFile allBookmarksExportFolder = new SubTemporaryFile(staging,
				MY_FAVOURITES_IMPORT_EXPORT_FOLDER);
			// write out the format details
			xmlHelper.writeExportFormatXmlFile(allBookmarksExportFolder, true);

			final XStream locXstream = getXStream();

			List<Bookmark> bookmarks = bookmarkDao.listAll();
			for( Bookmark bookmark : bookmarks )
			{
				initialiserService.initialise(bookmark);
				final BucketFile bucketFolder = new BucketFile(allBookmarksExportFolder, bookmark.getId());
				xmlHelper.writeXmlFile(bucketFolder, bookmark.getId() + ".xml", bookmark, locXstream);
			}
		}
	}

	@Override
	public void doImport(TemporaryFileHandle staging, Institution institution, ConverterParams params)
		throws IOException
	{
		final SubTemporaryFile allBookmarksImportFolder = new SubTemporaryFile(staging,
			MY_FAVOURITES_IMPORT_EXPORT_FOLDER);

		final XStream locXstream = getXStream();

		final List<String> entries = xmlHelper.getXmlFileList(allBookmarksImportFolder);
		for( String entry : entries )
		{
			Bookmark bookmark = xmlHelper.readXmlFile(allBookmarksImportFolder, entry, locXstream);
			bookmark.setInstitution(institution);
			bookmark.setId(0);

			Map<Long, Long> itemMap = params.getItems();
			Long newItemId = itemMap.get(bookmark.getItem().getId());
			bookmark.getItem().setId(newItemId.longValue());

			bookmarkDao.save(bookmark);
			bookmarkDao.flush();
			bookmarkDao.clear();
		}
	}

	@Override
	public String getStringId()
	{
		return "MYFAVOURITES";
	}

	private XStream getXStream()
	{
		if( xstream == null )
		{
			xstream = xmlHelper.createXStream();
			xstream.addDefaultImplementation(HashSet.class, Collection.class); // NOSONAR
			xstream.registerConverter(new IdOnlyConverter(Item.class));
		}
		return xstream;
	}
}