package com.tle.video.thumbnail;

import java.awt.Dimension;
import java.io.File;
import java.util.UUID;

import javax.inject.Inject;
import javax.inject.Singleton;

import com.tle.core.filesystem.StagingFile;
import com.tle.core.guice.Bind;
import com.tle.core.imagemagick.ImageMagickService;
import com.tle.core.imagemagick.ThumbnailOptions;
import com.tle.core.libav.LibAvService;
import com.tle.core.services.FileSystemService;
import com.tle.core.workflow.thumbnail.ThumbnailGenerator;
import com.tle.core.workflow.thumbnail.ThumbnailType;

@Bind
@Singleton
public class VideoThumbnailGenerator implements ThumbnailGenerator
{
	@Inject
	private FileSystemService fileSystemService;
	@Inject
	private ImageMagickService imageMagickService;
	@Inject
	private LibAvService libav;

	@Override
	public void generateThumbnail(File src, File dest) throws Exception
	{
		if( libav.isLibavInstalled() )
		{
			final StagingFile stagingFile = new StagingFile(UUID.randomUUID().toString());
			final File temp = fileSystemService.getExternalFile(stagingFile, src.getName()
				+ FileSystemService.THUMBNAIL_EXTENSION);
			try
			{
				fileSystemService.mkdir(stagingFile, null);
				libav.screenshotVideo(src, temp);
				imageMagickService.generateStandardThumbnail(temp, dest);
			}
			finally
			{
				fileSystemService.removeFile(stagingFile);
			}
		}
	}

	@Override
	public void generateThumbnailAdvanced(File srcFile, File dstFile, ThumbnailOptions options) throws Exception
	{
		if( libav.isLibavInstalled() )
		{
			final StagingFile stagingFile = new StagingFile(UUID.randomUUID().toString());
			final File temp = fileSystemService.getExternalFile(stagingFile, srcFile.getName()
				+ FileSystemService.THUMBNAIL_EXTENSION);
			try
			{
				fileSystemService.mkdir(stagingFile, null);
				libav.screenshotVideo(srcFile, temp);
				imageMagickService.generateThumbnailAdvanced(temp, dstFile, options);
			}
			finally
			{
				fileSystemService.removeFile(stagingFile);
			}
		}
	}

	@Override
	public Dimension getImageDimensions(File srcFile)
	{
		return null;
	}

	@Override
	public boolean supportsThumbType(ThumbnailType type)
	{
		return type != ThumbnailType.TYPE_GALLERY_PREVIEW;
	}

	@Override
	public boolean isEnabled()
	{
		return libav.isLibavInstalled();
	}
}