import cv2
import SimpleITK as sitk
import numpy as np
import pickle
import matplotlib.pyplot as plt
import os
import time
import rasterio
from rasterio.transform import from_origin


def apply_saved_transform(fixed_image_path, moving_image_path, transform_path):
    start_time = time.time()

    with open(transform_path, 'rb') as f:
        saved_transform = pickle.load(f)

    fixed_image_cv = cv2.imread(fixed_image_path, cv2.IMREAD_UNCHANGED)
    moving_image_cv = cv2.imread(moving_image_path, cv2.IMREAD_UNCHANGED)

    # Resize the thermal image to the same size as the optical image
    if fixed_image_cv.shape[0] > 1000 or fixed_image_cv.shape[1] > 1000:
        scale_percent = 50  # Reduce size by 50%
        width = int(fixed_image_cv.shape[1] * scale_percent / 100)
        height = int(fixed_image_cv.shape[0] * scale_percent / 100)
        dim = (width, height)
        fixed_image_cv_resized = cv2.resize(fixed_image_cv, dim)
        moving_image_cv_resized = cv2.resize(moving_image_cv, dim)
    else:
        moving_image_cv_resized = cv2.resize(moving_image_cv, (fixed_image_cv.shape[1], fixed_image_cv.shape[0]))
        fixed_image_cv_resized = fixed_image_cv

    # convert to simpleITK format
    fixed_image_sitk = sitk.GetImageFromArray(
        cv2.cvtColor(fixed_image_cv_resized, cv2.COLOR_BGR2GRAY).astype(np.float32))
    moving_image_sitk = sitk.GetImageFromArray(moving_image_cv_resized.astype(np.float32))

    # Ensure the types are the same
    if fixed_image_sitk.GetPixelID() != moving_image_sitk.GetPixelID():
        moving_image_sitk = sitk.Cast(moving_image_sitk, fixed_image_sitk.GetPixelID())

    moving_resampled = sitk.Resample(
        moving_image_sitk,
        fixed_image_sitk,
        saved_transform,
        sitk.sitkLinear,
        0.0,
        moving_image_sitk.GetPixelID()
    )

    moving_resampled_np = sitk.GetArrayFromImage(moving_resampled)
    moving_resampled_np = cv2.normalize(moving_resampled_np, None, 0, 255, cv2.NORM_MINMAX)
    moving_resampled_np = 255 - moving_resampled_np  # Invert the thermal image
    moving_resampled_colored = cv2.applyColorMap(moving_resampled_np.astype(np.uint8), cv2.COLORMAP_JET)

    # Display
    plt.figure()
    plt.subplot(1, 2, 1)
    plt.imshow(fixed_image_cv_resized)
    plt.title('Fixed Image')
    plt.subplot(1, 2, 2)
    plt.imshow(moving_resampled_colored)
    plt.title('Registered Moving Image')
    plt.show()

    # Combine the thermal and optical images into a single 5-band image
    overlay = fixed_image_cv_resized.astype(np.float32)
    output = moving_resampled_colored.astype(np.float32)
    cv2.addWeighted(overlay, 0.5, output, 0.5, 0, output)

    # Ensure the combined image is within the correct range
    output = np.clip(output, 0, 255).astype(np.uint8)

    # crop
    x_min, y_min, x_max, y_max = find_bounding_box(moving_resampled_colored)
    cropped_output = output[y_min:y_max, x_min:x_max]
    height, width, _ = output.shape

    # Check the size of the combined image
    print("Combined image shape:", output.shape)
    print("Combined  cropped image shape:", cropped_output.shape)


    end_time = time.time()
    elapsed_time = end_time - start_time
    print(f"Time taken for {os.path.splitext(fixed_image_filename)[0]}: {elapsed_time:.2f} seconds")

    return output, cropped_output

# crop
def find_bounding_box(image):
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    retval, thresh = cv2.threshold(gray, 50, 255, cv2.THRESH_BINARY)
    contours, hierarchy = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

    plt.title('Binary Mask')
    plt.imshow(thresh, cmap='gray')
    plt.show()

    print("Number of contours found = {}".format(len(contours)))

    if len(contours) == 0:
        # No contours found, return the full image
        return 0, 0, image.shape[1], image.shape[0]
    x_min, y_min, x_max, y_max = np.inf, np.inf, -np.inf, -np.inf

    for contour in contours:
        x, y, w, h = cv2.boundingRect(contour)
        x_min = min(x_min, x)
        y_min = min(y_min, y)
        x_max = max(x_max, x + w)
        y_max = max(y_max, y + h)
    return int(x_min), int(y_min), int(x_max), int(y_max)




total_start_time = time.time()  # Start time for total processing
base_dir = r"C:\Users\TEK1\OneDrive - Syracuse University\syracuse university\Thesis\pictures\Field july 2024\Images from Dorhea and Lepton"
subdirectories = [os.path.join(base_dir, d) for d in os.listdir(base_dir) if os.path.isdir(os.path.join(base_dir, d))]


for subdirectory in subdirectories:

    # Get the fixed (optical) and moving (thermal) image filenames
    nir_on_image_path = None
    nir_off_image_path = None
    moving_image_filename = None
    print(subdirectory)

    for filename in os.listdir(subdirectory):
        if filename.endswith('-NIR-OFF.jpg'):
            nir_off_image_path=os.path.join(subdirectory, filename)
        elif filename.endswith('-NIR-ON.jpg'):
            nir_on_image_path = os.path.join(subdirectory, filename)
        elif filename.endswith('.pgm'):
            moving_image_filename = filename


    moving_image_path = os.path.join(subdirectory, moving_image_filename)



    # Convert the PGM image to JPG with proper normalization
    image = cv2.imread(moving_image_path, cv2.IMREAD_UNCHANGED)
    image_normalized = cv2.normalize(image, None, 0, 255, cv2.NORM_MINMAX)
    image_normalized = image_normalized.astype(np.uint8)
    jpg_path = os.path.splitext(moving_image_path)[0] + '.jpg'
    cv2.imwrite(jpg_path, image_normalized)

    # Save the output images
    output_filename = f"registered.jpg"
    cropped_output_filename = f"cropped.jpg"
    final_image_filename = f"4_band_with_thermal.tif"
    # final_cropped_filename = f"4_band_with_thermal_cropped.tif"

    if not os.path.exists(output_filename) or not os.path.exists(cropped_output_filename):
        # Perform the registration and get the final transform
        transform, output, cropped_output, four_band = mutual_information_registration(nir_off_image_path, jpg_path)

        cv2.imwrite(os.path.join(subdirectory, output_filename), output)
        cv2.imwrite(os.path.join(subdirectory, cropped_output_filename), cropped_output)
        cv2.imwrite(os.path.join(subdirectory, final_image_filename), four_band)
        # cv2.imwrite(os.path.join(subdirectory, final_cropped_filename), four_band_cropped)
        W = four_band.shape[1]
        H = four_band.shape[0]

        # extract NIR and add as band
        NIR = cv2.imread(nir_on_image_path)
        NIR = cv2.cvtColor(NIR, cv2.COLOR_BGR2RGB)
        red_channel_NIR = NIR[:, :, 0]
        without_NIR = cv2.imread(nir_off_image_path)
        without_NIR = cv2.cvtColor(without_NIR, cv2.COLOR_BGR2RGB)
        red_channel = without_NIR[:, :, 0]
        Nir_band = cv2.subtract(red_channel_NIR, red_channel)
        Nir_band_resized = cv2.resize(Nir_band, (W, H))
        cv2.imwrite(os.path.join(subdirectory, "NIR band.png"), Nir_band_resized)

        final_five_band = np.dstack((four_band, Nir_band_resized))
        final_five_band = np.clip(final_five_band, 0, 255).astype(np.uint8)
        final_five_band_reordered = np.transpose(final_five_band, (2, 0, 1))
        final_path = os.path.join(subdirectory, "final_5_band.tif")

        # Save the final 5-band image as a TIFF
        with rasterio.open(final_path, 'w', driver='GTiff', height=final_five_band_reordered.shape[1],
                           width=final_five_band_reordered.shape[2], count=final_five_band_reordered.shape[0],
                           dtype=final_five_band_reordered.dtype, transform=transform) as dst:
            dst.write(final_five_band_reordered)

        print("final_five_band dtype:", final_five_band_reordered.dtype)

total_end_time = time.time()  # End time for total processing
total_elapsed_time = total_end_time - total_start_time
print(f"Total time taken for all image pairs: {total_elapsed_time:.2f} seconds")