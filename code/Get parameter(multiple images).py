import cv2
import SimpleITK as sitk
import numpy as np
import pickle
import os
import time

#import test

#test.mult()

def perform_registration(fixed_image_path, moving_image_path):
    start_time = time.time()
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

    #convert to simpleITK format
    fixed_image_sitk = sitk.GetImageFromArray(cv2.cvtColor(fixed_image_cv_resized, cv2.COLOR_BGR2GRAY).astype(np.float32))
    moving_image_sitk = sitk.GetImageFromArray(moving_image_cv_resized.astype(np.float32))

    #Ensure the types are the same
    if fixed_image_sitk.GetPixelID() != moving_image_sitk.GetPixelID():
        moving_image_sitk = sitk.Cast(moving_image_sitk, fixed_image_sitk.GetPixelID())

    #Initial the transform
    initial_transform = sitk.CenteredTransformInitializer(
        fixed_image_sitk,
        moving_image_sitk,
        sitk.Euler2DTransform(),
        sitk.CenteredTransformInitializerFilter.GEOMETRY
    )

    registration_method = sitk.ImageRegistrationMethod()
    registration_method.SetMetricAsMattesMutualInformation(numberOfHistogramBins=50)
    registration_method.SetInterpolator(sitk.sitkLinear)
    registration_method.SetOptimizerAsGradientDescent(learningRate=1.0, numberOfIterations=100)
    registration_method.SetOptimizerScalesFromPhysicalShift()
    registration_method.SetShrinkFactorsPerLevel(shrinkFactors = [4, 2, 1])
    registration_method.SetSmoothingSigmasPerLevel(smoothingSigmas=[2, 1, 0])
    registration_method.SmoothingSigmasAreSpecifiedInPhysicalUnitsOn()
    registration_method.SetInitialTransform(initial_transform, inPlace=False)

    final_transform = registration_method.Execute(
        sitk.Cast(fixed_image_sitk, sitk.sitkFloat32),
        sitk.Cast(moving_image_sitk, sitk.sitkFloat32)
    )
    end_time = time.time()
    elapsed_time = end_time - start_time
    print(f"Time taken for {os.path.splitext(fixed_image_filename)[0]}: {elapsed_time:.2f} seconds")
# saving the transform
    return final_transform


total_start_time = time.time()  # Start time for total processing
base_dir = r"C:\Users\TEK1\OneDrive - Syracuse University\syracuse university\Thesis\pictures\Field july 2024\Images from Dorhea and Lepton"
subdirectories = [os.path.join(base_dir, d) for d in os.listdir(base_dir) if os.path.isdir(os.path.join(base_dir, d))]


for subdirectory in subdirectories:

    transform = None

    for filename in os.listdir(subdirectory):
        if filename.endswith('.pkl'):
            transform.append(filename)


    if len(fixed_image_filenames) == 2 and moving_image_filename:
        moving_image_path = os.path.join(subdirectory, moving_image_filename)

        # Convert the PGM image to JPG with proper normalization
        image = cv2.imread(moving_image_path, cv2.IMREAD_UNCHANGED)
        image_normalized = cv2.normalize(image, None, 0, 255, cv2.NORM_MINMAX)
        image_normalized = image_normalized.astype(np.uint8)
        jpg_path = os.path.splitext(moving_image_path)[0] + '.jpg'
        cv2.imwrite(jpg_path, image_normalized)

        for fixed_image_filename in fixed_image_filenames:
            fixed_image_path = os.path.join(subdirectory, fixed_image_filename)

            Transform_filename= f"parameter_{os.path.splitext(fixed_image_filename)[0]}.pkl"

            if not os.path.exists(Transform_filename):

                transform = perform_registration(fixed_image_path, jpg_path)

                with open(os.path.join(subdirectory, Transform_filename), 'wb') as f:
                    pickle.dump(transform, f)

total_end_time = time.time()  # End time for total processing
total_elapsed_time = total_end_time - total_start_time
print(f"Total time taken for all image pairs: {total_elapsed_time:.2f} seconds")